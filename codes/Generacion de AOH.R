# ============================================================================
# SCRIPT PARA GENERAR AOHs DESDE LOS SDMs EN PIEDEMONTEMETA
# ============================================================================

# Cargar librerías
library(terra)
library(dplyr)
library(readr)
library(stringr)

# ===============================
# PARTE 1: CONFIGURACIÓN DE RUTAS
# ===============================

# Carpeta donde están los SDMs (PIEDEMONTEMETA)
dir_sdm <- "C:/Users/walter.garcia/Documents/STAR/recortados_MM"

# Carpeta donde guardaremos los AOHs
dir_aoh <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_MM/"
if (!dir.exists(dir_aoh)) {
  dir.create(dir_aoh, recursive = TRUE)
  cat("📁 Carpeta creada para AOHs:", dir_aoh, "\n")
}

# Archivo de metadata (opcional, solo para referencia)
csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies_MM.csv"

# ===============================
# PARTE 2: BUSCAR ARCHIVOS SDM EN PIEDEMONTEMETA
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("🔍 BUSCANDO ARCHIVOS SDM EN PIEDEMONTEMETA\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Buscar todos los archivos .tif en PiedemonteMeta
archivos_sdm <- list.files(dir_sdm, 
                           pattern = "\\.tif$", 
                           full.names = TRUE)

cat("Total archivos .tif encontrados:", length(archivos_sdm), "\n")

# Mostrar los primeros 10 archivos
cat("\nPrimeros 10 archivos:\n")
for (i in 1:min(10, length(archivos_sdm))) {
  cat(sprintf("  %d. %s\n", i, basename(archivos_sdm[i])))
}

# ===============================
# PARTE 3: USAR TODOS LOS ARCHIVOS
# ===============================

archivos_principales <- archivos_sdm

cat("\n", paste(rep("-", 40), collapse = ""), "\n")
cat("Archivos a procesar:", length(archivos_principales), "\n")


# ===============================
# PARTE 4: FUNCIÓN PARA EXTRAER NOMBRE DE ESPECIE
# ===============================

extraer_nombre_especie <- function(nombre_archivo) {
  # Limpiar el nombre del archivo
  nombre_limpio <- nombre_archivo %>%
    str_replace_all("\\.tif$", "") %>%
    str_replace_all("^V[0-9]_", "") %>%
    str_replace_all("_todos_|_foto_", "_") %>%
    str_replace_all("_MAXENT$|_BRT$|_RF$|_MaxEnt$", "") %>%
    str_replace_all("_", " ")
  
  # Lista de correcciones manuales para casos especiales
  correcciones <- c(
    "Aramides cajaneus" = "Aramides cajaneus",
    "Arremon taciturnus" = "Arremon taciturnus",
    "Arremonops conirostris" = "Arremonops conirostris",
    "Basileuterus culicivorus" = "Basileuterus culicivorus",
    "Bos taurus" = "Bos taurus",
    "Bubulcus ibis" = "Bubulcus ibis",
    "Cabassous unicinctus" = "Cabassous unicinctus",
    "Canis lupus familiaris" = "Canis lupus familiaris",
    "Canis familiaris" = "Canis familiaris",
    "Cantorchilus leucotis" = "Cantorchilus leucotis",
    "Catharus ustulatus" = "Catharus ustulatus",
    "Cerdocyon thous" = "Cerdocyon thous",
    "Coendou prehensilis" = "Coendou prehensilis",
    "Crotophaga ani" = "Crotophaga ani",
    "Crotophaga major" = "Crotophaga major",
    "Crypturellus cinereus" = "Crypturellus cinereus",
    "Crypturellus soui" = "Crypturellus soui",
    "Cuniculus paca" = "Cuniculus paca",
    "Cyanocorax violaceus" = "Cyanocorax violaceus",
    "Dasyprocta fuliginosa" = "Dasyprocta fuliginosa",
    "Dasypus novemcinctus" = "Dasypus novemcinctus",
    "Dendrocincla fuliginosa" = "Dendrocincla fuliginosa",
    "Didelphis canus" = "Didelphis canus",
    "Didelphis marsupialis" = "Didelphis marsupialis",
    "Eira barbara" = "Eira barbara",
    "Equus caballus" = "Equus caballus",
    "Felis catus" = "Felis catus",
    "Gallus gallus" = "Gallus gallus",
    "Geotrygon montana" = "Geotrygon montana",
    "Gymnomystax mexicanus" = "Gymnomystax mexicanus",
    "Iguana iguana" = "Iguana iguana",
    "Leopardus pardalis" = "Leopardus pardalis",
    "Leopardus wiedii" = "Leopardus wiedii",
    "Leptotila rufaxilla" = "Leptotila rufaxilla",
    "Leptotila verreauxi" = "Leptotila verreauxi",
    "Mesembrinibis cayennensis" = "Mesembrinibis cayennensis",
    "Momotus momota" = "Momotus momota",
    "Myrmecophaga tridactyla" = "Myrmecophaga tridactyla",
    "Myrmoborus myotherinus" = "Myrmoborus myotherinus",
    "Nasua nasua" = "Nasua nasua",
    "Nyctidromus albicollis" = "Nyctidromus albicollis",
    "Odocoileus cariacou" = "Odocoileus cariacou",
    "Odocoileus virginianus" = "Odocoileus virginianus",
    "Ortalis guttata" = "Ortalis guttata",
    "Penelope jacquacu" = "Penelope jacquacu",
    "Philander opossum" = "Philander opossum",
    "Phimosus infuscatus" = "Phimosus infuscatus",
    "Procyon cancrivorus" = "Procyon cancrivorus",
    "Psarocolius decumanus" = "Psarocolius decumanus",
    "Rupornis magnirostris" = "Rupornis magnirostris",
    "Saimiri sciureus" = "Saimiri sciureus",
    "Saltator maximus" = "Saltator maximus",
    "Sapajus apella" = "Sapajus apella",
    "Sciurus granatensis" = "Sciurus granatensis",
    "Syrigma sibilatrix" = "Syrigma sibilatrix",
    "Tamandua tetradactyla" = "Tamandua tetradactyla",
    "Turdus ignobilis" = "Turdus ignobilis",
    "Turdus leucomelas" = "Turdus leucomelas",
    "Turdus nudigenis" = "Turdus nudigenis",
    "Ardea alba" = "Ardea alba",
    "Burhinus bistriatus" = "Burhinus bistriatus",
    "Caracara cheriway" = "Caracara cheriway",
    "Galictis vittata" = "Galictis vittata",
    "Milvago chimachima" = "Milvago chimachima",
    "Odontophorus gujanensis" = "Odontophorus gujanensis",
    "Patagioenas cayennensis" = "Patagioenas cayennensis",
    "Phaethornis griseogularis" = "Phaethornis griseogularis",
    "Ramphocelus carbo" = "Ramphocelus carbo",
    "Vanellus chilensis" = "Vanellus chilensis"
  )
  
  # Buscar coincidencia en correcciones
  for (patron in names(correcciones)) {
    patron_sin_espacios <- gsub(" ", "_", patron)
    if (grepl(patron_sin_espacios, nombre_archivo, ignore.case = TRUE)) {
      return(correcciones[patron])
    }
  }
  
  return(nombre_limpio)
}

# ===============================
# PARTE 5: GENERAR AOHs
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("🌍 GENERANDO AOHs (Percentil 70%)\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Dataframe para registrar los AOHs generados
registro_aoh <- data.frame()

for (i in 1:length(archivos_principales)) {
  
  ruta_sdm <- archivos_principales[i]
  nombre_archivo <- basename(ruta_sdm)
  
  # Extraer nombre de especie
  nombre_especie <- extraer_nombre_especie(nombre_archivo)
  
  cat(sprintf("\n[%d/%d] Procesando: %s\n", i, length(archivos_principales), nombre_archivo))
  cat(sprintf("  Especie: %s\n", nombre_especie))
  
  # Leer SDM
  sdm <- try(rast(ruta_sdm))
  if (inherits(sdm, "try-error")) {
    cat("  ❌ Error al leer el archivo\n")
    next
  }
  
  # Mostrar información del raster
  cat(sprintf("  Dimensiones: %d x %d celdas\n", ncol(sdm), nrow(sdm)))
  cat(sprintf("  Resolución: %.4f\n", mean(res(sdm))))
  
  # Calcular percentil 70 para AOH
  vals <- values(sdm)
  thr <- quantile(vals, 0.7, na.rm = TRUE)
  cat(sprintf("  Threshold (70%%): %.4f\n", thr))
  
  # Crear AOH binario
  aoh <- sdm >= thr
  aoh <- as.numeric(aoh)
  
  # Estadísticas básicas
  freq_aoh <- freq(aoh)
  celdas_presencia <- ifelse(nrow(freq_aoh) > 1, freq_aoh[2,2], 0)
  cat(sprintf("  Celdas con presencia: %s\n", format(celdas_presencia, big.mark = ",")))
  
  # Generar nombre para el AOH
  nombre_aoh <- paste0(gsub(" ", "_", nombre_especie), "_AOH.tif")
  ruta_aoh <- file.path(dir_aoh, nombre_aoh)
  
  # Guardar AOH
  writeRaster(aoh, ruta_aoh, overwrite = TRUE)
  cat(sprintf("  ✅ AOH guardado: %s\n", nombre_aoh))
  
  # Registrar
  registro_aoh <- rbind(registro_aoh, data.frame(
    archivo_original = nombre_archivo,
    especie = nombre_especie,
    archivo_aoh = nombre_aoh,
    ruta_aoh = ruta_aoh,
    celdas_presencia = celdas_presencia,
    threshold = thr,
    fecha_generacion = Sys.time(),
    stringsAsFactors = FALSE
  ))
}

# ===============================
# PARTE 6: GUARDAR REGISTRO
# ===============================

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
cat("📊 RESUMEN FINAL\n")
cat(paste(rep("=", 60), collapse = ""), "\n")

# Guardar registro completo
registro_file <- file.path(dir_aoh, "Registro_AOHs_Completo.csv")
write.csv(registro_aoh, registro_file, row.names = FALSE)

# Mostrar resumen
cat("\n✅ PROCESO COMPLETADO\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
cat("Total archivos procesados:", nrow(registro_aoh), "\n")
cat("AOHs generados en:", dir_aoh, "\n")
cat("Registro guardado en:", registro_file, "\n")

# Listar los AOHs generados
cat("\n📁 AOHS GENERADOS:\n")
cat(paste(rep("-", 40), collapse = ""), "\n")
for (i in 1:nrow(registro_aoh)) {
  cat(sprintf("%3d. %s → %s celdas\n", 
              i, 
              registro_aoh$especie[i],
              format(registro_aoh$celdas_presencia[i], big.mark = ",")))
}

cat("\n", paste(rep("=", 60), collapse = ""), "\n", sep="")
