library(terra)
library(sf)
library(dplyr)
library(readr)
library(stringr)

# ===============================
# 1. RUTAS
# ===============================
dir_aoh  <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_MM"
csv_file <- "C:/Users/walter.garcia/Documents/STAR/especies_mm2.csv"
aoi_file <- "C:/Users/walter.garcia/Downloads/Teselas_Paisajes Focales Núcleos-20260409T192519Z-3-001/Teselas_Paisajes Focales Núcleos/Teselas_Nucleo_Magdalena_Medio/NMM_PF_Buffer_10km.shp"

# ===============================
# 2. METADATA
# ===============================
meta <- read_delim(
  csv_file,
  delim = ";",
  show_col_types = FALSE
)

meta$species_file <- basename(meta$species_file)

# ===============================
# 3. PESOS IUCN
# ===============================
iucn_weights <- c(
  "LC" = 0,
  "NT" = 100,
  "VU" = 200,
  "EN" = 300,
  "CR" = 400
)

meta$weight <- iucn_weights[meta$iucn_status]

# ===============================
# 4. TESELAS
# ===============================
teselas <- st_read(aoi_file, quiet = TRUE)

# ===============================
# 5. RASTERS
# ===============================
r_files <- list.files(
  dir_aoh,
  pattern = "\\.tif$",
  full.names = TRUE
)

if (length(r_files) == 0) {
  stop("❌ No se encontraron rasters .tif en: ", dir_aoh)
}

# ===============================
# 6. RASTER REFERENCIA
# ===============================
ref <- rast(r_files[1])

# ===============================
# 7. CRS TESELAS
# ===============================
teselas <- st_transform(
  teselas,
  crs(ref)
)

teselas_vect <- vect(teselas)

# ===============================
# 8. PRECALCULAR ÁREA GLOBAL
# ===============================
cat("📦 Calculando áreas globales...\n")

global_stats <- list()

for (f in r_files) {
  
  r <- rast(f)
  
  # ---------------------------
  # ALINEAR SI ES NECESARIO
  # ---------------------------
  if (!compareGeom(r, ref, stopOnError = FALSE)) {
    r <- project(r, ref, method = "near")
    r <- resample(r, ref, method = "near")
  }
  
  # ---------------------------
  # BINARIO
  # ---------------------------
  r_bin <- ifel(r == 1, 1, NA)
  
  # ---------------------------
  # GLOBAL CELLS
  # ---------------------------
  global_cells <- global(
    r_bin,
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  global_stats[[basename(f)]] <- global_cells
}

# ===============================
# 9. LOOP PRINCIPAL
# ===============================
resultados_lista <- vector(
  "list",
  length = nrow(teselas_vect)
)

for (i in seq_len(nrow(teselas_vect))) {
  
  poly <- teselas_vect[i]
  poly_id <- i
  
  cat(
    "\n🧩 Tesela",
    i,
    "/",
    nrow(teselas_vect),
    "\n"
  )
  
  resultados_poly <- list()
  
  # ---------------------------
  # LOOP ESPECIES
  # ---------------------------
  for (j in seq_along(r_files)) {
    
    r_file <- r_files[j]
    spname_file <- basename(r_file)
    
    # ---------------------------
    # METADATA (Cruce flexible por Species_2 y Género)
    # ---------------------------
    row_meta <- meta %>% filter(!is.na(species_file) & species_file == spname_file)
    
    if (nrow(row_meta) == 0) {
      row_meta <- meta %>% filter(sapply(Species_2, function(sp) grepl(sp, spname_file, ignore.case = TRUE)))
    }
    
    if (nrow(row_meta) == 0) {
      row_meta <- meta %>% filter(sapply(str_split(Species_2, "_")[[1]][1], function(gen) grepl(gen, spname_file, ignore.case = TRUE)))
    }
    
    if (nrow(row_meta) == 0) next
    
    row_meta_sel <- row_meta[1, ]
    peso <- row_meta_sel$weight
    
    if (is.na(peso) || peso == 0) next
    
    # ---------------------------
    # GLOBAL
    # ---------------------------
    global_cells <- global_stats[[spname_file]]
    
    if (is.na(global_cells) || global_cells == 0) next
    
    # ---------------------------
    # RASTER
    # ---------------------------
    r <- rast(r_file)
    
    # ---------------------------
    # ALINEAR SI ES NECESARIO
    # ---------------------------
    if (!compareGeom(r, ref, stopOnError = FALSE)) {
      r <- project(r, ref, method = "near")
      r <- resample(r, ref, method = "near")
    }
    
    # ---------------------------
    # BINARIO
    # ---------------------------
    r_bin <- ifel(r == 1, 1, NA)
    
    # ---------------------------
    # CROP + MASK
    # ---------------------------
    r_crop <- crop(r_bin, poly)
    
    if (ncell(r_crop) == 0) next
    
    r_mask <- mask(r_crop, poly)
    
    # ---------------------------
    # OVERLAP
    # ---------------------------
    overlap_cells <- global(
      r_mask,
      "sum",
      na.rm = TRUE
    )[1, 1]
    
    overlap_cells <- ifelse(
      is.na(overlap_cells),
      0,
      overlap_cells
    )
    
    if (overlap_cells == 0) next
    
    # ---------------------------
    # PRESENCIA
    # ---------------------------
    presence <- ifelse(
      overlap_cells > 0,
      1,
      0
    )
    
    # ---------------------------
    # START
    # ---------------------------
    START <- overlap_cells / global_cells
    
    # ---------------------------
    # STAR
    # ---------------------------
    STAR_species <- START * peso
    
    resultados_poly[[j]] <- data.frame(
      GRID_ID      = poly_id,
      species      = row_meta_sel$Species,
      file         = spname_file,
      iucn_status  = row_meta_sel$iucn_status,
      weight       = peso,
      global_cells = global_cells,
      overlap_cells= overlap_cells,
      presence     = presence,
      START        = START,
      STAR_species = STAR_species,
      stringsAsFactors = FALSE
    )
  }
  
  resultados_lista[[i]] <- bind_rows(resultados_poly)
}

# ===============================
# 10. UNIR RESULTADOS
# ===============================
resultados_final <- bind_rows(resultados_lista)

if (nrow(resultados_final) == 0) {
  stop("❌ No se encontraron intersecciones entre las teselas y los AOHs.")
}

# ===============================
# 11. STAR TOTAL
# ===============================
STAR_por_tesela <- resultados_final %>%
  group_by(GRID_ID) %>%
  summarise(
    STAR_total = sum(
      STAR_species,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ===============================
# 12. RIQUEZA SIMPLE
# ===============================
riqueza_simple <- resultados_final %>%
  group_by(GRID_ID) %>%
  summarise(
    richness = sum(
      presence,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ===============================
# 13. RIQUEZA PONDERADA
# ===============================
riqueza_ponderada <- resultados_final %>%
  group_by(GRID_ID) %>%
  summarise(
    richness_weighted = sum(
      weight * presence,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ===============================
# 14. FUSIONAR MÉTRICAS
# ===============================
fusion <- STAR_por_tesela %>%
  left_join(
    riqueza_simple,
    by = "GRID_ID"
  ) %>%
  left_join(
    riqueza_ponderada,
    by = "GRID_ID"
  )

# ===============================
# 15. NORMALIZAR
# ===============================
fusion$STAR_norm <- (
  fusion$STAR_total /
    max(fusion$STAR_total,
        na.rm = TRUE)
)

fusion$richness_norm <- (
  fusion$richness /
    max(fusion$richness,
        na.rm = TRUE)
)

fusion$richness_weighted_norm <- (
  fusion$richness_weighted /
    max(fusion$richness_weighted,
        na.rm = TRUE)
)

# ===============================
# 16. ÍNDICE HÍBRIDO (70% STAR, 30% Riqueza)
# ===============================
fusion$STAR_RICH <- (
  fusion$STAR_norm * 0.7 +
    fusion$richness_weighted_norm * 0.3
)

# ===============================
# 17. UNIR AL SHAPE
# ===============================
teselas$GRID_ID <- 1:nrow(teselas)

teselas_STAR <- teselas %>%
  left_join(
    fusion,
    by = "GRID_ID"
  )

# ===============================
# 18. EXPORTAR
# ===============================
st_write(
  teselas_STAR,
  "C:/Users/walter.garcia/Documents/STAR/STAR_RICH_teselas.shp",
  delete_dsn = TRUE,
  quiet = TRUE
)

write.csv(
  resultados_final,
  "C:/Users/walter.garcia/Documents/STAR/STAR_RICH_detalle.csv",
  row.names = FALSE
)

cat("\n============================================================\n")
cat("✅ STAR + riqueza completado y guardado con éxito\n")
cat("============================================================\n")