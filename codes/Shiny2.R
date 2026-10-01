# ================================
# LIBRERÍAS
# ================================
library(shiny)
library(terra)
library(sf)
library(dplyr)
library(readr)
library(DT)
library(stringr)

# ================================
# FUNCIÓN STAR (VERSIÓN FINAL ROBUSTA)
# ================================
calcular_star <- function(aoi = NULL, usar_poligono_prueba = FALSE){
  dir_aoh <- "C:/Users/walter.garcia/Documents/STAR/AOH_Generados_3"
  csv_file <- "C:/Users/walter.garcia/Documents/STAR/Especies_amenazadas.csv"
  
  meta <- read_delim(csv_file, delim = ";", show_col_types = FALSE)
  
  # Asegurar species_file
  if (!"species_file" %in% colnames(meta)){
    meta$species_file <- paste0(gsub(" ", "_", meta$Species), "_AOH.tif")
  }
  meta$species_file <- basename(meta$species_file)
  
  iucn_weights <- c("LC" = 0, "NT" = 100, "VU" = 200, "EN" = 300, "CR" = 400)
  meta$weight <- iucn_weights[meta$iucn_status]
  
  aoh_files <- list.files(dir_aoh, pattern = "_AOH\\.tif$", full.names = TRUE)
  if (length(aoh_files) == 0) stop("No hay AOH")
  
  ref_raster <- rast(aoh_files[1])
  
  # ==========================
  # AOI
  # ==========================
  if (usar_poligono_prueba || is.null(aoi)) {
    e <- ext(ref_raster)
    poly_matrix <- matrix(c(
      e[1], e[3],
      e[2], e[3],
      e[2], e[4],
      e[1], e[4],
      e[1], e[3]
    ), ncol = 2, byrow = TRUE)
    aoi_vect <- vect(poly_matrix, type = "polygon", crs = crs(ref_raster))
  } else {
    # 🔥 MANEJO CORRECTO sf → terra
    if (inherits(aoi, "sf")){
      aoi_vect <- vect(aoi)
    } else if (inherits(aoi, "SpatVector")){
      aoi_vect <- aoi
    } else {
      stop("Formato AOI no soportado")
    }
    
    aoi_vect <- project(aoi_vect, crs(ref_raster))
  }
  
  results_list <- list()
  
  # ==========================
  # LOOP PRINCIPAL
  # ==========================
  for (i in seq_along(aoh_files)) {
    aoh_file <- aoh_files[i]
    nombre_archivo <- basename(aoh_file)
    
    row_meta <- meta %>% filter(species_file == nombre_archivo)
    if (nrow(row_meta) == 0) next
    
    r <- rast(aoh_file)
    
    # Alinear geometría
    if (!compareGeom(r, ref_raster, stopOnError = FALSE)) {
      r <- project(r, ref_raster)
      r <- resample(r, ref_raster, method = "near")
    }
    
    # Binario robusto
    r_bin <- r > 0
    
    ca <- freq(r_bin)
    global_cells <- ifelse(any(ca$value == 1, na.rm = TRUE), ca$count[ca$value == 1], 0)
    if (global_cells == 0) next
    
    # Intersección
    r_crop <- crop(r_bin, aoi_vect)
    r_masked <- mask(r_crop, aoi_vect)
    
    ca_crop <- freq(r_masked)
    overlap_cells <- ifelse(any(ca_crop$value == 1, na.rm = TRUE), ca_crop$count[ca_crop$value == 1], 0)
    
    START <- overlap_cells / global_cells
    STAR_species <- START * row_meta$weight[1]
    
    results_list[[i]] <- data.frame(
      especie = row_meta$Species[1],
      iucn_status = row_meta$iucn_status[1],
      weight = row_meta$weight[1],
      global_celdas = global_cells,
      aoi_celdas = overlap_cells,
      START = START,
      STAR = STAR_species
    )
  }
  
  results <- bind_rows(results_list)
  return(results)
}

# ================================
# UI
# ================================
ui <- fluidPage(
  titlePanel("STAR Calculator"),
  sidebarLayout(
    sidebarPanel(
      h4("Ruta local"),
      textInput("ruta_shp", "Ruta del archivo .shp"),
      actionButton("load_ruta", "Cargar ruta"),
      
      hr(),
      h4("Subir shapefile"),
      fileInput("shp_multi", "Subir shapefile", multiple = TRUE),
      actionButton("load_upload", "Cargar archivos"),
      
      hr(),
      actionButton("run_star", "Calcular STAR"),
      actionButton("run_prueba", "Polígono prueba")
    ),
    mainPanel(
      DTOutput("tabla")
    )
  )
)

# ================================
# SERVER
# ================================
server <- function(input, output, session){
  aoi_data <- reactiveVal(NULL)
  resultados <- reactiveVal(NULL)
  
  # Ruta local
  observeEvent(input$load_ruta, {
    req(input$ruta_shp)
    tryCatch({
      aoi <- st_read(input$ruta_shp, quiet = TRUE)
      aoi_data(aoi)
      showNotification("AOI cargado correctamente", type = "message")
    }, error = function(e){
      showNotification(e$message, type = "error")
    })
  })
  
  # Upload
  observeEvent(input$load_upload, {
    req(input$shp_multi)
    files <- input$shp_multi
    temp <- tempdir()
    paths <- file.path(temp, files$name)
    file.copy(files$datapath, paths, overwrite = TRUE)
    
    shp <- paths[grepl("\\.shp$", paths)]
    if (length(shp) == 0) {
      showNotification("No se encontró .shp", type = "error")
      return()
    }
    
    aoi <- st_read(shp[1], quiet = TRUE)
    aoi_data(aoi)
    showNotification("AOI cargado correctamente", type = "message")
  })
  
  # Polígono prueba
  observeEvent(input$run_prueba, {
    res <- calcular_star(usar_poligono_prueba = TRUE)
    resultados(res)
  })
  
  # Ejecutar STAR
  observeEvent(input$run_star, {
    req(aoi_data())
    tryCatch({
      res <- calcular_star(aoi = aoi_data())
      resultados(res)
      showNotification(paste("Especies calculadas:", nrow(res)), type = "message")
    }, error = function(e){
      showNotification(e$message, type = "error")
    })
  })
  
  output$tabla <- renderDT({
    req(resultados())
    df <- resultados()
    
    if (!"aoi_celdas" %in% colnames(df)) {
      return(datatable(data.frame(Mensaje = "Sin resultados")))
    }
    
    df %>%
      filter(aoi_celdas > 0) %>%
      arrange(desc(STAR)) %>%
      datatable()
  })
}

# ================================
# RUN
# ================================
shinyApp(ui, server)
