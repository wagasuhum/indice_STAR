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


#### Area de habitat para *Dasypus novemcinctus* para varios años

<p align="center">
  <img src="https://wagasuhum.github.io/indice_STAR/assets/img/Dasypus.novemcinctus.png" width="600">
</p>

#### Area de habitat para *Dicotyles.tajacu* para varios años

<p align="center">
  <img src="https://wagasuhum.github.io/indice_STAR/assets/img/Dicotyles.tajacu.png" width="600">
</p>

### Guardado de raster de area de habitat
writeRaster(aoh, "Tinamus_major.tif", overwrite = TRUE)

aoh_m <- project(aoh, "EPSG:9377")  # Colombia Albers

area_km2 <- global(aoh_m, sum, na.rm = TRUE) * prod(res(aoh_m)) / 1e6
area_km2

aoh_por_tesela <- extract(aoh_m, teselas, fun = sum, na.rm = TRUE)
teselas$aoh_cells <- aoh_por_tesela[,2]



# Calculo Indice STAR


## Paquetes necesarios
library(terra)
library(sf)
library(dplyr)
library(readr)
library(stringr)


## Configuracion de rutas


### Carpeta donde estan los AOH generados
dir_aoh <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados/"

### Metadata de especies
csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies_3.csv"

### Area de interes
aoi_file <- "C:/Users/walter.garcia/Documents/STAR/poligono_pais_centrado.shp"



## Carga de metadata y pesos IUCN


meta <- read_delim(csv_file, delim = ";")

meta <- read_delim(csv_file, delim = ";")

### Pesos segun categoria IUCN
iucn_weights <- c(
  "LC" = 0,
  "NT" = 100,
  "VU" = 200,
  "EN" = 300,
  "CR" = 400
)

meta$weight <- iucn_weights[meta$iucn_status]



## Lectura de archivos AOH


aoh_files <- list.files(
  dir_aoh,
  pattern = "_AOH\\.tif$",
  full.names = TRUE
)

length(aoh_files)


## Carga del area de interes


aoi <- st_read(aoi_file)

### Raster de referencia
ref_raster <- rast(aoh_files[1])

### Reproyeccion del AOI
aoi_proj <- st_transform(aoi, crs(ref_raster))

### Creacion de mascara
mask_aoi <- rasterize(vect(aoi_proj), ref_raster, field = 1)


## Calculo del indice STAR


### dataframe donde se almacenaran los resultados finales
results <- data.frame()

### recorrer todos los rasters de AOH
for (i in 1:length(aoh_files)) {

  ### archivo raster actual
  aoh_file <- aoh_files[i]
  nombre_archivo <- basename(aoh_file)

  ### extraer nombre de la especie desde el nombre del archivo
  spname <- gsub("_AOH\\.tif$", "", nombre_archivo)
  spname <- gsub("_", " ", spname)

  ### buscar coincidencia exacta de la especie en la metadata
  row_meta <- meta %>%
    filter(tolower(trimws(Species)) == tolower(trimws(spname)))

  ### si no hay coincidencia, intentar usando solo genero y especie
  if (nrow(row_meta) == 0) {

    palabras <- str_split(spname, " ")[[1]]

    if (length(palabras) >= 2) {

      nombre_corto <- paste(palabras[1], palabras[2])

      row_meta <- meta %>%
        filter(grepl(nombre_corto, Species, ignore.case = TRUE))
    }
  }

  ### si no existe metadata para la especie se omite
  if (nrow(row_meta) == 0) next

  ### cargar raster AOH de la especie
  r <- rast(aoh_file)

  ### contar celdas de presencia en toda la distribucion
  ca <- freq(r_bin)

  global_cells <- ifelse(
    any(ca$value == 1, na.rm = TRUE),
    ca$count[ca$value == 1],
    0
  )

  ### si no hay celdas de presencia se omite
  if (global_cells == 0) next

  ### recortar raster usando el area de interes
  r_crop <- mask(r_bin, mask_aoi)

  ### contar celdas de presencia dentro del AOI
  ca_crop <- freq(r_crop)

  overlap_cells <- ifelse(
    any(ca_crop$value == 1, na.rm = TRUE),
    ca_crop$count[ca_crop$value == 1],
    0
  )

  ### proporcion del rango de la especie dentro del AOI
  START <- overlap_cells / global_cells

  ### calculo del indice STAR usando el peso IUCN
  STAR_species <- START * row_meta$weight[1]

  ### estimacion de area (suponiendo celdas de 1 km2 = 100 ha)
  area_ha_total <- global_cells * 100
  area_ha_aoi <- overlap_cells * 100

  ### guardar resultados de la especie
  results <- rbind(
    results,
    data.frame(
      especie = spname,
      iucn_status = row_meta$iucn_status[1],
      weight = row_meta$weight[1],
      global_area_ha = area_ha_total,
      aoi_area_ha = area_ha_aoi,
      START = START,
      STAR = STAR_species
    )
  )
}


## Resultados

STAR_total <- sum(results$STAR, na.rm = TRUE)

resumen_categoria <- results %>%
  group_by(iucn_status) %>%
  summarise(
    N_especies = n(),
    Area_ha = sum(aoi_area_ha),
    STAR = sum(STAR),
    .groups = "drop"
  )

## Desglose por Categoría IUCN

| Categoría IUCN | Nº Especies | Área (ha) | STAR |
|----------------|------------|-----------|------|
| CR | 2 | 649,600 | **13.1** |
| VU | 1 | 324,800 | **3.28** |
| NT | 1 | 324,800 | **1.64** |
| LC | 65 | 21,112,000 | 0 |

---

## Top 10 Especies con Mayor Contribución a STAR

| # | Especie | IUCN | Puntos | Área (ha) | START | STAR |
|---|--------|------|--------|-----------|-------|------|
| 1 | *Philander opossum* | CR | 400 | 360,300 | 0.0191 | **7.65** |
| 2 | *Myrmecophaga tridactyla* | VU | 200 | 167,300 | 0.0088 | **1.76** |
| 3 | *Leopardus wiedii* | NT | 100 | 195,600 | 0.0107 | **1.07** |
| 4 | *Aramides cajaneus* | LC | 0 | 4,978,100 | 0.0830 | 0 |
| 5 | *Ardea alba* | LC | 0 | 205,200 | 0.0114 | 0 |
| 6 | *Arremon taciturnus* | LC | 0 | 4,015,800 | 0.0759 | 0 |
| 7 | *Arremonops conirostris* | LC | 0 | 2,048,300 | 0.1109 | 0 |
| 8 | *Basileuterus culicivorus* | LC | 0 | 4,932,900 | 0.1170 | 0 |
| 9 | *Bos taurus* | LC | 0 | 91,700 | 0.0051 | 0 |
|10 | *Bubulcus ibis* | LC | 0 | 151,000 | 0.0083 | 0 |

---

