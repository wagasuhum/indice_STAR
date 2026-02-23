#  Calculo Indice STAR
## Area de Habitat
### paquetes necesarios
library(terra)

### Carga de modelo de distribucion

sdm <- rast("C:/Users/walter.garcia/Documents/STAR/PiedemonteCasanare/Tinamus_major_todos_MaxEnt.tif")  

### poligono de interes
teselas <- vect("C:/Users/walter.garcia/Documents/STAR/Teselas_10ha_Piedemonte_Casanare.shp")

### seleccion de intervalo para calcular el area de habitat
vals <- values(sdm)
thr <- quantile(vals, 0.7, na.rm = TRUE)
thr

### calculo de area de habitat
aoh <- sdm >= thr
aoh <- as.numeric(aoh)

### Validacion de valores
freq(aoh)

### Grafica de area de habitat
plot(aoh, col = c("purple", "yellow"))


<p align="center">
  <img src="https://wagasuhum.github.io/indice_STAR/assets/img/aoh_plot.png" width="600">
</p>


### Guardado de raster de area de habitat
writeRaster(aoh, "Tinamus_major.tif", overwrite = TRUE)

aoh_m <- project(aoh, "EPSG:9377")  # Colombia Albers

area_km2 <- global(aoh_m, sum, na.rm = TRUE) * prod(res(aoh_m)) / 1e6
area_km2

aoh_por_tesela <- extract(aoh_m, teselas, fun = sum, na.rm = TRUE)
teselas$aoh_cells <- aoh_por_tesela[,2]


## Calculo de STAR

### Cargar librerías
library(terra)
library(sf)
library(dplyr)
library(readr)

### configuracion inicial 
dir_rasters <- "C:/Users/walter.garcia/Documents/STAR/PiedemonteMeta/"
csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies.csv"
aoi_file <- "C:/Users/walter.garcia/Documents/STAR/poligono_pais_centrado.shp"

### Leer metadata
meta <- read_delim(csv_file, delim = ";")
meta$species_file <- basename(meta$species_file)

### Asignar pesos IUCN SEGÚN DOCUMENTACIÓN STAR
iucn_weights <- c("LC" = 0, "NT" = 100, "VU" = 200, "EN" = 300, "CR" = 400)
meta$weight <- iucn_weights[meta$iucn_status]

if (any(is.na(meta$weight))) {
  warning("⚠ Algunas especies no tienen estado IUCN válido en 'meta'.")
}

###  Leer Area de interes (shapefile)
aoi <- st_read(aoi_file)

### Preparar rasters de especies

r_files <- list.files(dir_rasters, pattern = "\\.tif$", full.names = TRUE)

### Reproyectar AOI al CRS de los rasters
ref <- rast(r_files[1])
aoi_proj <- st_transform(aoi, crs(ref))
mask_aoi <- rasterize(vect(aoi_proj), ref, field = 1)

### Loop por especies

results <- data.frame()

for (r_file in r_files) {
  spname <- basename(r_file)
  row_meta <- meta %>% filter(species_file == spname)
  
  if (nrow(row_meta) == 0) {
    warning(paste("⚠ No hay metadata para:", spname))
    next
  }
  
  #### Leer raster
  r <- rast(r_file)
  
  #### Binarizar (1 = presencia, NA = ausencia)
  r_bin <- r
  r_bin[r > 0] <- 1
  r_bin[r <= 0 | is.na(r)] <- NA
  
  #### Contar celdas globales
  ca <- freq(r_bin)
  global_cells <- ifelse(any(ca$value == 1), ca$count[ca$value == 1], 0)
  
  #### Recortar con AOI
  r_crop <- mask(r_bin, mask_aoi)
  ca_crop <- freq(r_crop)
  overlap_cells <- ifelse(any(ca_crop$value == 1), ca_crop$count[ca_crop$value == 1], 0)
  
  #### Calcular START para esta especie (proporción de superposición)
  START <- ifelse(global_cells > 0, overlap_cells / global_cells, NA)
  
  #### CORRECCIÓN: Calcular STAR para esta especie según metodología oficial
  #### STAR = START * Peso_IUCN
  STAR_species <- START * row_meta$weight
  
  
  ### Guardar resultados
  results <- rbind(results, data.frame(
    species_id    = row_meta$Species_id,
    species       = row_meta$Species,
    file          = spname,
    iucn_status   = row_meta$iucn_status,
    weight        = row_meta$weight,
    global_cells  = global_cells,
    overlap_cells = overlap_cells,
    START         = START,
    STAR_species  = STAR_species  # STAR individual por especie
  ))
}

### 5. Calcular índices globales CORREGIDOS


#### STAR total = Suma de todos los STAR individuales
STAR_total <- sum(results$STAR_species, na.rm = TRUE)

#### También calcular por categoría IUCN
STAR_by_category <- results %>%
  group_by(iucn_status) %>%
  summarise(
    n_especies = n(),
    STAR_categoria = sum(STAR_species, na.rm = TRUE),
    .groups = 'drop'
  )

#### Mostrar resultados CORREGIDOS

cat("==================================================\n")
cat("📊 RESULTADOS FINALES - ANÁLISIS STAR (CORREGIDO)\n")
cat("==================================================\n")
cat("Total especies procesadas:", nrow(results), "\n")
cat("Total archivos raster encontrados:", length(r_files), "\n")
cat("STAR TOTAL (según metodología oficial):", round(STAR_total, 2), "\n")
cat("==================================================\n\n")

### Resultados por categoría IUCN
cat("DESGLOSE POR CATEGORÍA IUCN:\n")
cat("----------------------------------------\n")
for (i in 1:nrow(STAR_by_category)) {
  cat(sprintf("%s: %2d especies, STAR=%8.2f\n", 
              STAR_by_category$iucn_status[i], 
              STAR_by_category$n_especies[i],
              STAR_by_category$STAR_categoria[i]))
}

cat("\n")

### Mostrar las 10 especies con mayor contribución a STAR
cat("TOP 10 ESPECIES - MAYOR CONTRIBUCIÓN A STAR:\n")
cat("----------------------------------------\n")
top_star <- results[order(-results$STAR_species), ][1:10, ]
for (i in 1:nrow(top_star)) {
  cat(sprintf("%2d. %-30s START=%6.4f Peso=%3d STAR=%7.2f\n", 
              i, top_star$species[i], top_star$START[i], 
              top_star$weight[i], top_star$STAR_species[i]))
}

