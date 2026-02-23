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
  <img src="assets/img/aoh_plot.png">
</p>

### Guardado de raster de area de habitat
writeRaster(aoh, "Tinamus_major.tif", overwrite = TRUE)

aoh_m <- project(aoh, "EPSG:9377")  # Colombia Albers

area_km2 <- global(aoh_m, sum, na.rm = TRUE) * prod(res(aoh_m)) / 1e6
area_km2

aoh_por_tesela <- extract(aoh_m, teselas, fun = sum, na.rm = TRUE)
teselas$aoh_cells <- aoh_por_tesela[,2]
