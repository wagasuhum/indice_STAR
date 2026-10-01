library(terra)
library(sf)
library(dplyr)
library(readr)

# ===============================
# 1. RUTAS
# ===============================
dir_aoh <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_3"

csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies_amenazadas.csv"

aoi_file <- "C:/Users/walter.garcia/Documents/STAR/STAR_test_10_density.shp"

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
# 4. TESSELAS
# ===============================
teselas <- st_read(aoi_file, quiet = TRUE)

# ===============================
# 5. RASTERS
# ===============================
r_files <- list.files(
  dir_aoh,
  pattern = "_AOH\\.tif$",
  full.names = TRUE
)

if (length(r_files) == 0) {
  stop("❌ No hay AOHs")
}

# ===============================
# 6. RASTER REFERENCIA
# ===============================
ref <- rast(r_files[1])

# ===============================
# 7. CRS TESSELAS
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
  # ALINEAR SI NECESARIO
  # ---------------------------
  if (!compareGeom(r, ref,
                   stopOnError = FALSE)) {
    
    r <- project(
      r,
      ref,
      method = "near"
    )
    
    r <- resample(
      r,
      ref,
      method = "near"
    )
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
  )[1,1]
  
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
    # METADATA
    # ---------------------------
    row_meta <- meta %>%
      filter(species_file == spname_file)
    
    if (nrow(row_meta) == 0) next
    
    peso <- row_meta$weight[1]
    
    if (is.na(peso) || peso == 0) next
    
    # ---------------------------
    # RASTER
    # ---------------------------
    r <- rast(r_file)
    
    # ---------------------------
    # ALINEAR SI NECESARIO
    # ---------------------------
    if (!compareGeom(r, ref,
                     stopOnError = FALSE)) {
      
      r <- project(
        r,
        ref,
        method = "near"
      )
      
      r <- resample(
        r,
        ref,
        method = "near"
      )
    }
    
    # ---------------------------
    # BINARIO
    # ---------------------------
    r_bin <- ifel(r == 1, 1, NA)
    
    # ---------------------------
    # GLOBAL
    # ---------------------------
    global_cells <- global_stats[[spname_file]]
    
    if (is.na(global_cells) ||
        global_cells == 0) next
    
    # ---------------------------
    # CROP + MASK
    # ---------------------------
    r_crop <- crop(r_bin, poly)
    
    r_mask <- mask(r_crop, poly)
    
    # ---------------------------
    # OVERLAP
    # ---------------------------
    overlap_cells <- global(
      r_mask,
      "sum",
      na.rm = TRUE
    )[1,1]
    
    overlap_cells <- ifelse(
      is.na(overlap_cells),
      0,
      overlap_cells
    )
    
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
      GRID_ID = poly_id,
      species = row_meta$Species[1],
      file = spname_file,
      iucn_status = row_meta$iucn_status[1],
      weight = peso,
      global_cells = global_cells,
      overlap_cells = overlap_cells,
      presence = presence,
      START = START,
      STAR_species = STAR_species
    )
  }
  
  resultados_lista[[i]] <- bind_rows(resultados_poly)
}

# ===============================
# 10. UNIR RESULTADOS
# ===============================
resultados_final <- bind_rows(resultados_lista)

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
# 16. ÍNDICE HÍBRIDO
# ===============================

# Balance 70% STAR
# 30% riqueza ponderada

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

cat("\n✅ STAR + riqueza completado\n")



# mapa --------------------------------------------------------------------


library(sf)
library(ggplot2)

# Leer shapefile
teselas <- st_read("C:/Users/walter.garcia/Documents/STAR/STAR riqueza/STAR_RICH_teselas.shp")

# Ver nombres de campos
names(teselas)


library(ggplot2)
library(sf)

mapa <- ggplot(teselas) +
  geom_sf(
    aes(fill = rchnss_w),
    color = "black",
    linewidth = 0.1
  ) +
  scale_fill_viridis_c(
    name = "Valor"
  ) +
  theme_minimal()

print(mapa)

library(sf)
library(dplyr)
library(ggplot2)

teselas <- teselas %>%
  mutate(
    clase = cut(
      rchnss_w,
      breaks = c(0, 1600, 2200, 2600, 3600, 4600),
      include.lowest = TRUE,
      labels = c(
        "0 - 1600",
        "1600 - 2200",
        "2200 - 2600",
        "2600 - 3600",
        "3600 - 4600"
      )
    )
  )

mapa <- ggplot(teselas) +
  
  geom_sf(
    aes(fill = clase),
    color = "black",
    linewidth = 0.05
  ) +
  
  scale_fill_manual(
    values = c(
      "0 - 1600" = "#d80027",
      "1600 - 2200" = "#f4a582",
      "2200 - 2600" = "#f7f7f7",
      "2600 - 3600" = "#bdbdbd",
      "3600 - 4600" = "#4d4d4d"
    ),
    drop = FALSE
  ) +
  
  theme_void() +
  
  theme(
    legend.position = "left",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  
  labs(fill = NULL)

print(mapa)




library(grid)

mapa <- ggplot(teselas) +
  
  geom_sf(
    aes(fill = clase),
    color = "black",
    linewidth = 0.05
  ) +
  
  scale_fill_manual(
    values = c(
      "0 - 1600" = "#d80027",
      "1600 - 2200" = "#f4a582",
      "2200 - 2600" = "#f7f7f7",
      "2600 - 3600" = "#bdbdbd",
      "3600 - 4600" = "#4d4d4d"
    ),
    drop = FALSE,
    name = NULL
  ) +
  
  labs(
    title = "Riqueza ponderada por tesela"
  ) +
  
  theme_minimal() +
  
  theme(
    plot.title = element_text(
      hjust = 0,
      size = 12,
      face = "plain"
    ),
    
    legend.position = "left",
    
    legend.key.height = unit(0.8, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    
    panel.grid.major = element_line(
      colour = "grey85",
      linewidth = 0.3
    ),
    
    panel.background = element_rect(
      fill = "white",
      colour = NA
    ),
    
    plot.background = element_rect(
      fill = "white",
      colour = NA
    )
  )

print(mapa)


ggsave(
  "Mapa_riqueza.png",
  plot = mapa,
  width = 10,
  height = 7,
  dpi = 300,
  bg = "white"
)
