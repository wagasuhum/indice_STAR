# ============================================================================
# SCRIPT PARA CALCULAR STAR USANDO LOS AOHS GENERADOS
# ============================================================================

# Cargar librerías
library(terra)
library(sf)
library(dplyr)
library(readr)
library(stringr)

# ===============================
# PARTE 1: CONFIGURACIÓN DE RUTAS
# ===============================

# Carpeta donde están los AOHs generados
dir_aoh <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_3"

# Archivo de metadata con categorías IUCN
csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies_amenazadas.csv"

# Shapefile del área de interés (polígono país o teselas)
aoi_file <- "C:/Users/walter.garcia/Documents/STAR/area_estudio/area_estudio.shp"

# ===============================
# PARTE 2: LEER METADATA Y PESOS IUCN
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("📋 CARGANDO METADATA Y PESOS IUCN\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Leer metadata
meta <- read_delim(csv_file, delim = ";")

# Definir pesos IUCN
iucn_weights <- c("LC" = 0, "NT" = 100, "VU" = 200, "EN" = 300, "CR" = 400)

# Asignar pesos
meta$weight <- iucn_weights[meta$iucn_status]

# Orden UICN de mayor amenaza a menor
meta$iucn_status <- factor(
  meta$iucn_status,
  levels = c("CR", "EN", "VU", "NT", "LC"),
  ordered = TRUE
)

# Verificar
cat("\nCategorías IUCN en metadata:\n")
print(table(meta$iucn_status))

cat("\nEspecies con categorías > LC:\n")
especies_no_lc <- meta %>% filter(iucn_status != "LC")
print(especies_no_lc[, c("Species", "iucn_status", "weight")])

# ===============================
# PARTE 3: LEER ARCHIVOS AOH
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("🔍 BUSCANDO ARCHIVOS AOH\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Buscar todos los archivos AOH
aoh_files <- list.files(dir_aoh, pattern = "_AOH\\.tif$", full.names = TRUE)

cat("Total archivos AOH encontrados:", length(aoh_files), "\n")

if (length(aoh_files) == 0) {
  stop("❌ No se encontraron archivos AOH en: ", dir_aoh)
}

# Mostrar primeros archivos
cat("\nPrimeros 10 AOHs:\n")
for (i in 1:min(10, length(aoh_files))) {
  cat(sprintf("  %d. %s\n", i, basename(aoh_files[i])))
}

# ===============================
# PARTE 4: PREPARAR ÁREA DE INTERÉS (AOI)
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("🗺️  PREPARANDO ÁREA DE INTERÉS\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Leer AOI
if (!file.exists(aoi_file)) {
  stop("❌ No se encuentra el archivo AOI: ", aoi_file)
}

aoi <- st_read(aoi_file)
cat("✅ AOI cargado:", nrow(aoi), "polígonos\n")

# Usar el primer AOH como referencia para CRS
ref_raster <- rast(aoh_files[1])
cat("CRS de referencia:", crs(ref_raster, describe=TRUE)$name, "\n")

# Reproyectar AOI al CRS de los rasters
aoi_proj <- st_transform(aoi, crs(ref_raster))
cat("✅ AOI reproyectado\n")

# Crear máscara del AOI
mask_aoi <- rasterize(vect(aoi_proj), ref_raster, field = 1)
cat("✅ Máscara AOI creada\n")

# ===============================
# PARTE 5: CALCULAR STAR PARA CADA ESPECIE
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("📊 CALCULANDO STAR\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

aoh_files <- file.path(dir_aoh, meta$species_file)
aoh_files <- aoh_files[file.exists(aoh_files)]

# Dataframe para resultados
results <- data.frame()

for (i in 1:length(aoh_files)) {
  
  aoh_file <- aoh_files[i]
  nombre_archivo <- basename(aoh_file)
  
  # Extraer nombre de especie del nombre del archivo
  # Extraer solo "Genero especie"
  spname <- gsub(".*?([A-Z][a-z]+_[a-z]+).*", "\\1", nombre_archivo)
  spname <- gsub("_", " ", spname)
  
  # Buscar en metadata
  row_meta <- meta %>% filter(tolower(trimws(Species)) == tolower(trimws(spname)))
  
  # Si no encuentra, intentar con el nombre científico (primeras dos palabras)
  if (nrow(row_meta) == 0) {
    palabras <- str_split(spname, " ")[[1]]
    if (length(palabras) >= 2) {
      nombre_corto <- paste(palabras[1], palabras[2])
      row_meta <- meta %>% filter(grepl(nombre_corto, Species, ignore.case = TRUE))
    }
  }
  
  if (nrow(row_meta) == 0) {
    warning("⚠ No hay metadata para: ", spname, " - Archivo: ", nombre_archivo)
    next
  }
  
  # Mostrar progreso
  cat(sprintf("\n[%d/%d] %s | IUCN: %s | Peso: %d\n", 
              i, length(aoh_files), 
              spname, 
              row_meta$iucn_status[1],
              row_meta$weight[1]))
  
  # Leer AOH
  r <- rast(aoh_file)
  

  
  # Contar celdas globales (toda el área de distribución)
  ca <- freq(r)
  global_cells <- ifelse(any(ca$value == 1, na.rm = TRUE), 
                         ca$count[ca$value == 1], 0)
  
  if (global_cells == 0) {
    cat("  ⚠ Sin celdas de presencia\n")
    next
  }
  
  # Recortar con AOI para contar celdas dentro del área de interés
  # Convertir AOI a SpatVector una sola vez
  aoi_vect <- vect(aoi_proj)
  
  # Dentro del loop:
  r_crop <- crop(r, aoi_vect)
  r_masked <- mask(r_crop, aoi_vect)
  
  ca_crop <- freq(r_crop)
  overlap_cells <- ifelse(any(ca_crop$value == 1, na.rm = TRUE), 
                          ca_crop$count[ca_crop$value == 1], 0)
  
  # Calcular START (proporción del rango en el AOI)
  START <- overlap_cells / global_cells
  
  # Calcular STAR = START * Peso IUCN
  STAR_species <- START * row_meta$weight[1]
  
  # Calcular área aproximada (asumiendo celdas de 1km² = 100 ha)
  # Ajusta según la resolución real de tus rasters
  area_ha_total <- global_cells * 100
  area_ha_aoi <- overlap_cells * 100
  
  cat(sprintf("  Global: %s celdas (%s ha)\n", 
              format(global_cells, big.mark = ","),
              format(round(area_ha_total), big.mark = ",")))
  cat(sprintf("  AOI: %s celdas (%s ha)\n", 
              format(overlap_cells, big.mark = ","),
              format(round(area_ha_aoi), big.mark = ",")))
  cat(sprintf("  START: %.4f\n", START))
  cat(sprintf("  STAR: %.2f\n", STAR_species))
  
  # Guardar resultados
  results <- rbind(results, data.frame(
    especie = spname,
    nombre_archivo = nombre_archivo,
    species_id = row_meta$Species_id[1],
    species_meta = row_meta$Species[1],
    iucn_status = row_meta$iucn_status[1],
    weight = row_meta$weight[1],
    global_celdas = global_cells,
    global_area_ha = area_ha_total,
    aoi_celdas = overlap_cells,
    aoi_area_ha = area_ha_aoi,
    START = START,
    STAR = STAR_species,
    stringsAsFactors = FALSE
  ))
}

# ===============================
# PARTE 6: RESULTADOS FINALES
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat(" RESULTADOS FINALES STAR\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

if (nrow(results) > 0) {
  
  STAR_total <- sum(results$STAR, na.rm = TRUE)
  
  cat("\n ESTADÍSTICAS GENERALES:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat(sprintf("Especies analizadas:      %d\n", nrow(results)))
  cat(sprintf("STAR TOTAL:               %.2f\n", STAR_total))
  
  # Resumen por categoría IUCN
  cat("\n DESGLOSE POR CATEGORÍA IUCN:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  
  resumen_categoria <- results %>%
    group_by(iucn_status) %>%
    summarise(
      N_especies = n(),
      Area_ha = sum(aoi_area_ha),
      STAR = sum(STAR),
      .groups = 'drop'
    ) %>%
    arrange(iucn_status)
  
  print(resumen_categoria)
  
  # Top 10 especies por STAR
  cat("\n TOP 10 ESPECIES - MAYOR CONTRIBUCIÓN A STAR:\n")
  cat(paste(rep("-", 60), collapse = ""), "\n")
  
  top10 <- results %>%
    arrange(desc(STAR)) %>%
    head(10) %>%
    mutate(rank = row_number())
  
  for (i in 1:nrow(top10)) {
    cat(sprintf("%2d. %-35s %3s | %3d pts | %8s ha | START: %.4f | STAR: %7.2f\n",
                top10$rank[i],
                substr(top10$especie[i], 1, 35),
                top10$iucn_status[i],
                top10$weight[i],
                format(round(top10$aoi_area_ha[i]), big.mark = ","),
                top10$START[i],
                top10$STAR[i]))
  }
  
  # Especies con categorías especiales que no se procesaron
  especies_procesadas <- unique(results$species_meta)
  especies_faltantes <- especies_no_lc %>% 
    filter(!Species %in% especies_procesadas)
  
  if (nrow(especies_faltantes) > 0) {
    cat("\n⚠ ESPECIES CON CATEGORÍA ESPECIAL NO PROCESADAS:\n")
    cat(paste(rep("-", 40), collapse = ""), "\n")
    print(especies_faltantes[, c("Species", "iucn_status")])
  }
  
  # ===============================
  # PARTE 7: GUARDAR RESULTADOS
  # ===============================
  
  # Guardar resultados completos
  output_file <- file.path(dir_aoh, "Resultados_STAR_Completos.csv")
  write.csv(results, output_file, row.names = FALSE)
  
  # Guardar resumen
  summary_file <- file.path(dir_aoh, "Resumen_STAR.csv")
  write.csv(resumen_categoria, summary_file, row.names = FALSE)
  
  cat("\n✅ ARCHIVOS GUARDADOS:\n")
  cat(paste(rep("-", 40), collapse = ""), "\n")
  cat("1. Resultados completos:", output_file, "\n")
  cat("2. Resumen por categoría:", summary_file, "\n")
  
} else {
  cat("\n❌ No se obtuvieron resultados. Verifica que los nombres en los AOHs coincidan con la metadata.\n")
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("✅ PROCESO COMPLETADO\n")
cat(paste(rep("=", 60), collapse = ""), "\n")




# mapa --------------------------------------------------------------------


plot(r, col = gray.colors(100, alpha = 0.2), legend = FALSE)

plot(aoi_vect,
     add = TRUE,
     border = "blue",
     lwd = 4,
     col = rgb(0, 0, 1, 0.2))  # relleno semitransparente




# por teselas -------------------------------------------------------------

# =========================================
# STAR POR TESELAS (VERSIÓN ROBUSTA)
# =========================================

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
meta <- read_delim(csv_file, delim = ";", show_col_types = FALSE)
meta$species_file <- basename(meta$species_file)

iucn_weights <- c("LC" = 0, "NT" = 100, "VU" = 200, "EN" = 300, "CR" = 400)
meta$weight <- iucn_weights[meta$iucn_status]

# ===============================
# 3. TESELAS (AOI)
# ===============================
teselas <- st_read(aoi_file)

# ===============================
# 4. RASTERS
# ===============================
r_files <- list.files(dir_aoh, pattern = "_AOH\\.tif$", full.names = TRUE)

if (length(r_files) == 0) stop("❌ No hay AOHs")

# Raster de referencia
ref <- rast(r_files[1])

# Reproyectar teselas
teselas <- st_transform(teselas, crs(ref))
teselas_vect <- vect(teselas)

# ===============================
# 5. LOOP PRINCIPAL
# ===============================
resultados_lista <- vector("list", length = nrow(teselas))

for (i in seq_len(10)) {
  
  poly <- teselas_vect[i]
  poly_id <- i   # si tienes GRID_ID úsalo aquí
  
  cat("\n🧩 Tesela", i, "/", nrow(teselas_vect), "\n")
  
  resultados_poly <- list()
  
  # ---------------------------
  # Loop especies
  # ---------------------------
  for (j in seq_along(r_files)) {
    
    r_file <- r_files[j]
    spname_file <- basename(r_file)
    
    row_meta <- meta %>% filter(species_file == spname_file)
    
    if (nrow(row_meta) == 0) next
    
    peso <- row_meta$weight[1]
    if (peso == 0) next
    
    r <- rast(r_file)
    
    # 🔥 CLAVE: alinear raster al de referencia
    if (!compareGeom(r, ref, stopOnError = FALSE)) {
      r <- project(r, ref)
      r <- resample(r, ref, method = "bilinear")
    }
    
    # Binario
    r_bin <- r == 1
    
    # Global
    ca <- freq(r_bin)
    global_cells <- ifelse(any(ca$value == 1, na.rm = TRUE),
                           ca$count[ca$value == 1], 0)
    
    if (global_cells == 0) next
    
    # Recorte robusto (SIN rasterizar)
    r_crop <- crop(r_bin, poly)
    r_mask <- mask(r_crop, poly)
    
    ca_crop <- freq(r_mask)
    overlap_cells <- ifelse(any(ca_crop$value == 1, na.rm = TRUE),
                            ca_crop$count[ca_crop$value == 1], 0)
    
    START <- overlap_cells / global_cells
    #print(overlap_cells)
    
    
    STAR_species <- START * peso
    
    resultados_poly[[j]] <- data.frame(
      GRID_ID       = poly_id,
      species       = row_meta$Species[1],
      file          = spname_file,
      iucn_status   = row_meta$iucn_status[1],
      weight        = peso,
      global_cells  = global_cells,
      overlap_cells = overlap_cells,
      START         = START,
      STAR_species  = STAR_species
    )
  }
  
  resultados_lista[[i]] <- bind_rows(resultados_poly)
}

# ===============================
# 6. UNIR RESULTADOS
# ===============================
resultados_final <- bind_rows(resultados_lista)

# STAR por tesela
STAR_por_tesela <- resultados_final %>%
  group_by(GRID_ID) %>%
  summarise(STAR_total = sum(STAR_species, na.rm = TRUE), .groups = "drop")

# ===============================
# 7. UNIR CON SHAPE
# ===============================
teselas$GRID_ID <- 1:nrow(teselas)

teselas_STAR <- teselas %>%
  left_join(STAR_por_tesela, by = "GRID_ID")

# ===============================
# 8. GUARDAR
# ===============================
st_write(teselas_STAR,
         "C:/Users/walter.garcia/Documents/STAR/STAR_teselas.shp",
         delete_dsn = TRUE)

write.csv(resultados_final,
          "C:/Users/walter.garcia/Documents/STAR/STAR_teselas_detalle.csv",
          row.names = FALSE)

cat("\n✅ STAR por teselas completado\n")






# cluters -----------------------------------------------------------------

# Cargar librería
library(sf)

# Ruta del shapefile
shp_path <- "C:/Users/walter.garcia/Documents/STAR/STAR_test_10.shp"

# Leer shapefile
shp <- st_read(shp_path)
coords <- st_coordinates(st_centroid(shp))

km <- kmeans(cbind(coords, shp$STAR_total), centers = 10)

shp$cluster <- as.factor(km$cluster)

plot(shp["cluster"])

aggregate(shp$STAR_total, by = list(shp$cluster), FUN = mean)


# cobertura ---------------------------------------------------------------


# =========================================
# COBERTURA DE ESPECIES EN EL AOI
# =========================================

# Total especies esperadas
total_species <- meta %>%
  distinct(Species) %>%
  nrow()

# Especies representadas en el AOI
species_aoi <- results %>%
  filter(aoi_celdas > 0) %>%
  distinct(species_meta) %>%
  nrow()

# Cobertura
coverage <- species_aoi / total_species
coverage_pct <- coverage * 100

cat("\n COBERTURA DE ESPECIES:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")

cat(sprintf("Especies totales metadata: %d\n", total_species))
cat(sprintf("Especies presentes AOI:    %d\n", species_aoi))
cat(sprintf("Cobertura del AOI:         %.2f %%\n", coverage_pct))
