# =============================================================================
# STAR POR TESELAS + AMENAZA DOMINANTE (VERSIÓN DEFINITIVA Y ROBUSTA)
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(readr)
library(ggplot2)

if(!require(exactextractr)){
  install.packages("exactextractr")
  library(exactextractr)
}

# =============================================================================
# 1. RUTAS Y CONFIGURACIÓN
# =============================================================================

dir_aoh      <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_MM"
csv_file     <- "C:/Users/walter.garcia/Documents/STAR/especies_mm2.csv"
teselas_file <- "C:/Users/walter.garcia/Downloads/Teselas_Paisajes Focales Núcleos-20260409T192519Z-3-001/Teselas_Paisajes Focales Núcleos/Teselas_Nucleo_Magdalena_Medio/NMM_PF_Buffer_10km.shp"
dir_pressure <- "C:/Users/walter.garcia/Documents/STAR/Presiones"
output_dir   <- "C:/Users/walter.garcia/Documents/STAR/Resultados_STAR_Teselas_MM"

if(!dir.exists(output_dir)){
  dir.create(output_dir, recursive = TRUE)
}

# =============================================================================
# 2. CARGAR METADATA Y PESOS IUCN
# =============================================================================

meta <- read_delim(csv_file, delim = ";", show_col_types = FALSE)

iucn_weights <- c(
  "LC" = 0,
  "NT" = 100,
  "VU" = 200,
  "EN" = 300,
  "CR" = 400
)

meta$weight <- iucn_weights[meta$iucn_status]

# Limpiar espacios en blanco de la columna Species del CSV
meta$Species_clean <- trimws(tolower(meta$Species))

# =============================================================================
# 3. ARCHIVOS AOH
# =============================================================================

aoh_files <- list.files(dir_aoh, pattern = "_AOH\\.tif$", full.names = TRUE)

if(length(aoh_files) == 0){
  stop("No se encontraron rasters AOH en la carpeta especificada.")
}

ref_raster <- rast(aoh_files[1])

# =============================================================================
# 4. CARGAR TESELAS Y REPROYECTAR
# =============================================================================

teselas <- st_read(teselas_file, quiet = TRUE)
teselas <- st_transform(teselas, crs(ref_raster))
teselas$ID <- 1:nrow(teselas)

teselas_vect <- vect(teselas)

# =============================================================================
# 5. CARGAR CAPAS DE PRESIÓN
# =============================================================================

pressure_files <- list.files(dir_pressure, pattern = "\\.tif$", full.names = TRUE)

if(length(pressure_files) == 0){
  stop("No se encontraron capas de presión.")
}

pressure_rasters <- list()

for(f in pressure_files){
  nombre <- tools::file_path_sans_ext(basename(f))
  nombre <- make.names(nombre)
  
  r <- rast(f)
  if(!same.crs(r, ref_raster)){
    r <- project(r, ref_raster)
  }
  pressure_rasters[[nombre]] <- r
}

normalize_raster <- function(r){
  vals <- values(r, na.rm = TRUE)
  vals <- vals[is.finite(vals)]
  if(length(vals) == 0) return(r * 0)
  rmin <- min(vals)
  rmax <- max(vals)
  if(rmax == rmin) return(r * 0)
  return((r - rmin) / (rmax - rmin))
}

pressure_norm <- lapply(pressure_rasters, normalize_raster)

# =============================================================================
# 6. INICIALIZAR CAMPOS DE SALIDA EN TESELAS
# =============================================================================

teselas$STAR_TOTAL <- 0
teselas$STAR_FRAG  <- 0
teselas$STAR_LU    <- 0
teselas$STAR_POP   <- 0
teselas$STAR_VIAS  <- 0

# =============================================================================
# 7. BUCLE PRINCIPAL DE PROCESAMIENTO POR ESPECIE
# =============================================================================

cat("\n============================================================\n")
cat("PROCESANDO ESPECIES Y CALCULANDO STAR POR TESELA\n")
cat("============================================================\n")

for(i in seq_along(aoh_files)){
  
  aoh_path <- aoh_files[i]
  nombre_archivo <- basename(aoh_path)
  
  # --- Extracción con Expresión Regular (Género_especie) ---
  coincidencia <- regmatches(
    nombre_archivo, 
    regexpr("[A-Z][a-z]+_[a-z]+", nombre_archivo)
  )
  
  if(length(coincidencia) == 0){
    next
  }
  
  spname_espacio <- gsub("_", " ", coincidencia)
  spname_snake   <- coincidencia
  
  # --- Buscar coincidencia en metadata ---
  row_meta <- meta %>% 
    filter(
      Species_clean == trimws(tolower(spname_espacio)) |
        tolower(trimws(Species_2)) == tolower(spname_snake) |
        grepl(tolower(spname_snake), tolower(species_file), fixed = TRUE)
    )
  
  if(nrow(row_meta) == 0){
    cat(sprintf("[%d/%d] Especie: '%s' (de %s) -> Sin metadata en CSV\n", i, length(aoh_files), spname_espacio, nombre_archivo))
    next
  }
  
  peso <- row_meta$weight[1]
  cat(sprintf("[%d/%d] Especie encontrada: %s (%s, Peso: %d)\n", i, length(aoh_files), row_meta$Species[1], row_meta$iucn_status[1], peso))
  
  # --- Cargar AOH de la especie ---
  r <- rast(aoh_path)
  r[r != 1] <- NA
  
  # Celdas globales
  global_cells <- global(!is.na(r), "sum", na.rm = TRUE)[1, 1]
  if(is.na(global_cells) || global_cells == 0){
    cat("  -> Sin presencia global (celdas vacías)\n")
    next
  }
  
  # Verificar solape con teselas
  if(!is.related(r, teselas_vect, "intersects")){
    cat("  -> Sin solape geográfico con el área de teselas\n")
    next
  }
  
  # --- Extracción en teselas ---
  cov_list <- tryCatch({
    exact_extract(r, teselas, fun = "sum", progress = FALSE)
  }, error = function(e) NULL)
  
  if(is.null(cov_list)) next
  cov_list[is.na(cov_list)] <- 0
  
  if(sum(cov_list) == 0){
    cat("  -> Sin presencia dentro de las teselas de estudio\n")
    next
  }
  
  # STAR vectorizado por tesela
  start_vec <- cov_list / global_cells
  star_vec  <- start_vec * peso
  
  # Acumular STAR Total
  teselas$STAR_TOTAL <- teselas$STAR_TOTAL + star_vec
  
  # --- Extraer Presiones re-alineando dinámicamente si difieren las geometrías ---
  for(p in names(pressure_norm)){
    rp <- pressure_norm[[p]]
    
    # Re-alinear si las geometrías de la capa de presión y del AOH difieren
    if(!same.crs(rp, r) || !compareGeom(rp, r, stopOnError = FALSE)){
      rp <- resample(rp, r, method = "bilinear")
    }
    
    # Promedio ponderado de la presión dentro de la presencia de la especie
    p_mean <- tryCatch({
      exact_extract(rp, teselas, fun = "mean", weights = r, progress = FALSE)
    }, error = function(e) NULL)
    
    if(is.null(p_mean)) next
    p_mean[is.na(p_mean)] <- 0
    
    star_p_vec <- star_vec * p_mean
    
    if(p == "frag2022")      teselas$STAR_FRAG <- teselas$STAR_FRAG + star_p_vec
    else if(p == "LU12022")  teselas$STAR_LU   <- teselas$STAR_LU   + star_p_vec
    else if(p == "Pop2022")  teselas$STAR_POP  <- teselas$STAR_POP  + star_p_vec
    else if(p == "Vias2022") teselas$STAR_VIAS <- teselas$STAR_VIAS + star_p_vec
  }
}

# =============================================================================
# 8. DETERMINAR AMENAZA DOMINANTE POR TESELA
# =============================================================================

cat("\nCalculando amenaza dominante por tesela...\n")

teselas <- teselas %>%
  rowwise() %>%
  mutate(
    Suma_Amenazas = sum(c(STAR_FRAG, STAR_LU, STAR_POP, STAR_VIAS), na.rm = TRUE),
    Amenaza_Dominante = if_else(
      Suma_Amenazas == 0,
      "Sin Amenaza / Presencia",
      c("Fragmentacion", "Uso_Suelo", "Poblacion", "Vias")[
        which.max(c(STAR_FRAG, STAR_LU, STAR_POP, STAR_VIAS))
      ]
    )
  ) %>%
  ungroup()

# =============================================================================
# 9. MAPA Y EXPORTACIÓN
# =============================================================================

colores_amenazas <- c(
  "Fragmentacion"           = "#d73027",
  "Uso_Suelo"               = "#fc8d59",
  "Poblacion"               = "#4575b4",
  "Vias"                    = "#1a9850",
  "Sin Amenaza / Presencia" = "#cccccc"
)

mapa <- ggplot(teselas) +
  geom_sf(aes(fill = Amenaza_Dominante), color = "white", linewidth = 0.05) +
  scale_fill_manual(values = colores_amenazas) +
  theme_minimal() +
  labs(
    title = "Amenaza Dominante por Tesela (STAR)",
    fill  = "Categoría"
  )

st_write(
  teselas,
  file.path(output_dir, "STAR_Teselas_Amenaza_Dominante.shp"),
  delete_layer = TRUE
)

ggsave(
  file.path(output_dir, "Mapa_Amenaza_Dominante.png"),
  mapa,
  width = 10,
  height = 8,
  dpi = 300
)

cat("\n¡Proceso finalizado exitosamente!\nResultados en:", output_dir, "\n")


print(mapa)

summary(teselas$STAR_POP)

max(teselas$STAR_POP, na.rm = TRUE)

sum(teselas$STAR_POP > 0, na.rm = TRUE)


summary(teselas$STAR_FRAG)
summary(teselas$STAR_LU)
summary(teselas$STAR_VIAS)


table(teselas$Amenaza_Dominante)

round(
  100 * prop.table(
    table(teselas$Amenaza_Dominante)
  ),
  2
)


# =============================================================================
# GENERAR TABLA DE PRESIONES PROMEDIO POR ESPECIE
# =============================================================================

library(terra)
library(sf)
library(dplyr)
library(readr)

# 1. Rutas
dir_aoh      <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_MM"
csv_file     <- "C:/Users/walter.garcia/Documents/STAR/especies_mm2.csv"
dir_pressure <- "C:/Users/walter.garcia/Documents/STAR/Presiones"
output_dir   <- "C:/Users/walter.garcia/Documents/STAR/Resultados_MM_presiones"

if(!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

# 2. Cargar Metadata y Rasters
meta <- read_delim(csv_file, delim = ";", show_col_types = FALSE)
aoh_files <- list.files(dir_aoh, pattern = "_AOH\\.tif$", full.names = TRUE)
ref_raster <- rast(aoh_files[1])

# 3. Cargar y Normalizar Capas de Presión
pressure_files <- list.files(dir_pressure, pattern = "\\.tif$", full.names = TRUE)
pressure_norm <- list()

for(f in pressure_files){
  nombre <- tools::file_path_sans_ext(basename(f))
  nombre <- make.names(nombre)
  r <- rast(f)
  if(!same.crs(r, ref_raster)) r <- project(r, ref_raster)
  
  # Normalizar 0 - 1
  vals <- values(r, na.rm = TRUE)
  vals <- vals[is.finite(vals)]
  rmin <- min(vals); rmax <- max(vals)
  r_norm <- (r - rmin) / (rmax - rmin)
  
  pressure_norm[[nombre]] <- r_norm
}

# 4. Procesar Especie por Especie
lista_resultados <- list()

for(i in seq_along(aoh_files)){
  
  aoh_path <- aoh_files[i]
  nombre_archivo <- basename(aoh_path)
  
  # Extraer Nombre Científico
  coincidencia <- regmatches(nombre_archivo, regexpr("[A-Z][a-z]+_[a-z]+", nombre_archivo))
  if(length(coincidencia) == 0) next
  
  spname_espacio <- gsub("_", " ", coincidencia)
  spname_snake   <- coincidencia
  
  # Buscar Categoria IUCN en CSV (si no existe asigna "Sin metadata")
  row_meta <- meta %>% 
    filter(
      tolower(trimws(Species)) == tolower(spname_espacio) |
        tolower(trimws(Species_2)) == tolower(spname_snake)
    )
  
  cat_iucn <- if(nrow(row_meta) > 0) as.character(row_meta$iucn_status[1]) else "NE/Sin Meta"
  
  # Cargar AOH
  r <- rast(aoh_path)
  r[r != 1] <- NA
  
  if(global(!is.na(r), "sum", na.rm=TRUE)[1,1] == 0) next
  
  # Fila base de resultados
  res_row <- data.frame(
    ESPECIE = spname_espacio,
    CATEGORIA = cat_iucn,
    stringsAsFactors = FALSE
  )
  
  # Extraer Promedios de Presión
  for(p in names(pressure_norm)){
    rp <- pressure_norm[[p]]
    
    if(!compareGeom(rp, r, stopOnError = FALSE)){
      rp <- resample(rp, r, method = "bilinear")
    }
    
    # Extraer valores de la presión sobre la presencia de la especie
    vals_p <- values(rp)[!is.na(values(r))]
    vals_p <- vals_p[!is.na(vals_p) & is.finite(vals_p)]
    
    val_mean <- if(length(vals_p) > 0) round(mean(vals_p), 3) else 0
    
    # Renombrar columnas amigables
    col_name <- case_when(
      p == "frag2022" ~ "FRAGMENTACION",
      p == "LU12022"  ~ "USO_DEL_SUELO",
      p == "Pop2022"  ~ "POBLACION",
      p == "Vias2022" ~ "VIAS",
      TRUE ~ p
    )
    
    res_row[[col_name]] <- val_mean
  }
  
  lista_resultados[[length(lista_resultados) + 1]] <- res_row
}

# 5. Unir y Exportar Tabla
tabla_especies <- bind_rows(lista_resultados) %>% distinct(ESPECIE, .keep_all = TRUE)

print(head(tabla_especies, 10))

write.csv(
  tabla_especies, 
  file.path(output_dir, "Tabla_Especies_Presiones_Completa.csv"), 
  row.names = FALSE
)

cat("\n¡Tabla generada con éxito en!:", file.path(output_dir, "Tabla_Especies_Presiones_Completa.csv"))


library(dplyr)

tabla_porcentajes <- tabla_especies %>%
  rowwise() %>%
  mutate(
    Suma_Total = sum(c(FRAGMENTACION, USO_DEL_SUELO, POBLACION, VIAS), na.rm = TRUE),
    
    # Calcular porcentajes sobre el total de presión acumulada
    Pct_Fragmentacion = round((FRAGMENTACION / Suma_Total) * 100, 1),
    Pct_Uso_Suelo     = round((USO_DEL_SUELO / Suma_Total) * 100, 1),
    Pct_Poblacion     = round((POBLACION / Suma_Total) * 100, 1),
    Pct_Vias          = round((VIAS / Suma_Total) * 100, 1)
  ) %>%
  ungroup() %>%
  select(ESPECIE, CATEGORIA, Pct_Fragmentacion, Pct_Uso_Suelo, Pct_Poblacion, Pct_Vias)

# Ver resultado
print(head(tabla_porcentajes))

# Exportar
write.csv(tabla_porcentajes, "Tabla_Especies_Presiones_Porcentaje.csv", row.names = FALSE)