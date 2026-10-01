library(terra)

r <- rast("C:/Users/walter.garcia/Documents/STAR/recortados/clip_19012023_Apistogramma_macmasteri_MAXENT.tif")
plot(r)


library(terra)

# Raster máscara
mask_raster <- rast("C:/Users/walter.garcia/Documents/STAR/PiedemonteMeta/Aramides_cajaneus_todos_BRT.tif")

# Carpeta con los tif a recortar
input_folder <- "C:/Users/walter.garcia/Documents/STAR/Modelos finales/Final"

# Carpeta de salida
output_folder <- "C:/Users/walter.garcia/Documents/STAR/recortados"
dir.create(output_folder, showWarnings = FALSE)

# Listar archivos
files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

for (f in files) {
  
  r <- rast(f)
  
  # Asegurar misma proyección
  if (!crs(r) == crs(mask_raster)) {
    r <- project(r, mask_raster)
  }
  
  # Asegurar misma resolución/extensión
  r <- resample(r, mask_raster)
  
  # Aplicar máscara
  r_masked <- mask(r, mask_raster)
  
  # Guardar
  out_name <- file.path(output_folder, paste0("clip_", basename(f)))
  writeRaster(r_masked, out_name, overwrite = TRUE)
}



# casanare ----------------------------------------------------------------

library(terra)

# Descargar Colombia nivel departamentos
colombia <- geodata::gadm(country = "COL", level = 1, path = tempdir())

# Ver nombres (IMPORTANTE)
unique(colombia$NAME_1)

# Filtrar Boyacá y Casanare
deps <- colombia[colombia$NAME_1 %in% c("Boyaca", "Casanare"), ]


deps_union <- aggregate(deps)

input_folder <- "C:/Users/walter.garcia/Documents/STAR/Modelos finales/Final"

output_folder <- "C:/Users/walter.garcia/Documents/STAR/recortados4"

dir.create(output_folder, showWarnings = FALSE)

files <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

for (f in files) {
  
  r <- rast(f)
  
  # Reproyectar si es necesario
  if (!crs(r) == crs(deps_union)) {
    deps_proj <- project(deps_union, crs(r))
  } else {
    deps_proj <- deps_union
  }
  
  # Recorte + máscara
  r_crop <- crop(r, deps_proj)
  r_masked <- mask(r_crop, deps_proj)
  
  out_name <- file.path(output_folder, paste0("clip_", basename(f)))
  writeRaster(r_masked, out_name, overwrite = TRUE)
}



# municpios ---------------------------------------------------------------


library(terra)
library(geodata)

# Descargar municipios de Colombia (nivel 2)
municipios <- geodata::gadm(
  country = "COL",
  level = 2,
  path = tempdir()
)

# Ver nombres de departamentos y municipios
unique(municipios$NAME_1)   # departamentos
unique(municipios$NAME_2)   # municipios

# -----------------------------
# MUNICIPIO EN BOYACÁ
# Ejemplo: Tunja
# -----------------------------
cubara <- municipios[
  municipios$NAME_1 == "Boyacá" &
    municipios$NAME_2 == "Cubará",
]

# -----------------------------
# MUNICIPIO EN CASANARE
# Ejemplo: Yopal
# -----------------------------
paz <- municipios[
  municipios$NAME_1 == "Casanare" &
    municipios$NAME_2 == "Aguazul",
]

# Ver capas
plot(cubara, col = "lightblue")
plot(paz, col = "lightgreen")

# -----------------------------
# UNIR EN UNA SOLA CAPA
# -----------------------------
dos_municipios <- rbind(cubara, paz)

plot(dos_municipios,
     col = c("lightblue", "lightgreen"))

expanse(paz, unit = "km")


library(terra)

# Cargar shapefile
shape <- vect("C:/Users/walter.garcia/Documents/STAR/area_estudio/area_estudio.shp")

# Ver información
shape

# Calcular área en km²
expanse(shape, unit = "km")





# grafico -----------------------------------------------------------------


library(terra)
library(geodata)

# =========================
# Área de estudio
# =========================
shape <- vect(
  "C:/Users/walter.garcia/Documents/STAR/area_estudio/area_estudio.shp"
)

# =========================
# Departamentos Colombia
# =========================
departamentos <- geodata::gadm(
  country = "COL",
  level = 1,
  path = tempdir()
)

# Boyacá y Casanare
deps <- departamentos[
  departamentos$NAME_1 %in% c("Boyacá", "Casanare"),
]

# =========================
# Municipios Colombia
# =========================
municipios <- geodata::gadm(
  country = "COL",
  level = 2,
  path = tempdir()
)

# Cubará - Boyacá
cubara <- municipios[
  municipios$NAME_1 == "Boyacá" &
    municipios$NAME_2 == "Cubará",
]

# Aguazul - Casanare
Monterrey <- municipios[
  municipios$NAME_1 == "Casanare" &
    municipios$NAME_2 == "Monterrey",
]

# =========================
# Extensión del mapa
# =========================
e <- ext(rbind(shape, deps, cubara, Monterrey))

# =========================
# MAPA
# =========================
par(bg = "white")

# Departamentos fondo
plot(
  deps,
  ext = e + 0.3,
  col = gray.colors(2, alpha = 0.15),
  border = "gray50",
  lwd = 1.5
)

# Área de estudio
plot(
  shape,
  add = TRUE,
  col = rgb(0, 0.6, 0, 0.25),
  border = "darkgreen",
  lwd = 2.5
)

# Cubará
plot(
  cubara,
  add = TRUE,
  col = rgb(0, 0, 1, 0.30),
  border = "blue",
  lwd = 3
)

# Aguazul
plot(
  Monterrey,
  add = TRUE,
  col = rgb(1, 0, 0, 0.30),
  border = "red",
  lwd = 3
)

# =========================
# Etiquetas municipios
# =========================
text(
  centroids(cubara),
  labels = "Cubará",
  col = "blue",
  font = 2,
  cex = 0.9
)

text(
  centroids(aguazul),
  labels = "Aguazul",
  col = "red",
  font = 2,
  cex = 0.9
)

# =========================
# Etiquetas departamentos
# =========================
text(
  centroids(deps),
  labels = deps$NAME_1,
  col = "gray30",
  font = 2,
  cex = 1
)

# =========================
# Leyenda
# =========================
legend(
  "bottomleft",
  legend = c(
    "Área de estudio",
    "Cubará",
    "Aguazul"
  ),
  fill = c(
    rgb(0, 0.6, 0, 0.25),
    rgb(0, 0, 1, 0.30),
    rgb(1, 0, 0, 0.30)
  ),
  border = c(
    "darkgreen",
    "blue",
    "red"
  ),
  bty = "n",
  cex = 0.9
)


library(terra)

# Guardar Cubará
writeVector(
  cubara,
  "C:/Users/walter.garcia/Documents/STAR/cubara.shp",
  overwrite = TRUE
)

# Guardar Aguazul
writeVector(
  Monterrey,
  "C:/Users/walter.garcia/Documents/STAR/monterrey.shp",
  overwrite = TRUE
)


# Magdalena medio ----------------------------------------------------------------

library(terra)
library(geodata)

# ---------------------------------------------------------
# 1. Departamentos de interés
# ---------------------------------------------------------

deps <- colombia[colombia$NAME_1 %in%
                   c("Norte de Santander",
                     "Santander",
                     "Cesar"), ]

# Unir los departamentos
deps_union <- aggregate(deps)

# ---------------------------------------------------------
# 2. Carpetas
# ---------------------------------------------------------

input_folder <- "C:/Users/walter.garcia/Documents/STAR/Modelos finales/Final"

output_folder <- "C:/Users/walter.garcia/Documents/STAR/recortados_MM"

dir.create(
  output_folder,
  showWarnings = FALSE,
  recursive = TRUE
)

# ---------------------------------------------------------
# 3. Buscar GeoTIFF
# ---------------------------------------------------------

files <- list.files(
  input_folder,
  pattern = "\\.tif$",
  full.names = TRUE,
  ignore.case = TRUE
)

cat("GeoTIFF encontrados:", length(files), "\n")

# ---------------------------------------------------------
# 4. Procesar cada raster
# ---------------------------------------------------------

for (f in files) {
  
  cat("\n========================================\n")
  cat("Procesando:", basename(f), "\n")
  
  tryCatch({
    
    # Leer raster
    r <- rast(f)
    
    # -----------------------------------------------
    # Comprobar CRS
    # -----------------------------------------------
    
    if (is.na(crs(r)) || crs(r) == "") {
      
      cat("ERROR: El raster NO tiene CRS\n")
      cat("Se omite:", basename(f), "\n")
      
      next
    }
    
    cat("CRS del raster:\n")
    print(crs(r))
    
    # -----------------------------------------------
    # Transformar departamentos al CRS del raster
    # -----------------------------------------------
    
    deps_proj <- project(
      deps_union,
      crs(r)
    )
    
    # -----------------------------------------------
    # Comprobar extensiones
    # -----------------------------------------------
    
    cat("Extensión raster:\n")
    print(ext(r))
    
    cat("Extensión departamentos:\n")
    print(ext(deps_proj))
    
    # -----------------------------------------------
    # Comprobar si se intersectan
    # -----------------------------------------------
    
    e1 <- ext(r)
    e2 <- ext(deps_proj)
    
    overlap <- 
      e1$xmax >= e2$xmin &&
      e1$xmin <= e2$xmax &&
      e1$ymax >= e2$ymin &&
      e1$ymin <= e2$ymax
    
    if (!overlap) {
      
      cat("\n*** SIN INTERSECCIÓN ***\n")
      cat("El raster no cubre Norte de Santander/Santander/Cesar.\n")
      cat("Se omite:", basename(f), "\n")
      
      next
    }
    
    # -----------------------------------------------
    # Recortar
    # -----------------------------------------------
    
    r_crop <- crop(
      r,
      deps_proj
    )
    
    # -----------------------------------------------
    # Aplicar máscara
    # -----------------------------------------------
    
    r_masked <- mask(
      r_crop,
      deps_proj
    )
    
    # -----------------------------------------------
    # Nombre de salida
    # -----------------------------------------------
    
    out_name <- file.path(
      output_folder,
      paste0("clip_", basename(f))
    )
    
    # -----------------------------------------------
    # Guardar
    # -----------------------------------------------
    
    writeRaster(
      r_masked,
      out_name,
      overwrite = TRUE
    )
    
    cat("OK:", out_name, "\n")
    
  }, error = function(e) {
    
    cat("\n*** ERROR EN ARCHIVO ***\n")
    cat(basename(f), "\n")
    cat("Mensaje:", e$message, "\n")
    
  })
}
