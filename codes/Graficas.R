library(tidyverse)
library(readxl)
library(readr)
library(stringi)


datos <- read_delim(
  "Listado_MM.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)

datos <- datos %>%
  mutate(
    Endemica = stri_trans_general(Endemica, "Latin-ASCII"),
    Endemica = trimws(Endemica),
    endemica_cat = case_when(
      Endemica == "Endemica" ~ "Endémica",
      is.na(Endemica) ~ "No endémica",
      TRUE ~ "No endémica"
    )
  )


datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(Endemica) %>%
  ggplot(aes(x = Endemica, y = n)) +
  geom_col() +
  labs(
    title = "Especies amenazadas según endemismo",
    x = "",
    y = "Número de especies"
  ) +
  theme_minimal()



datos %>%
  filter(
    Amenazada_Global == "Amenazada",
    Endemica == "Endemica"
  ) %>%
  count(Clase, sort = TRUE) %>%
  ggplot(aes(x = reorder(Clase, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Clases taxonómicas de especies amenazadas y endémicas",
    x = "Clase",
    y = "Número de especies"
  ) +
  theme_minimal()


datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(Clase) %>%
  ggplot(aes(x = reorder(Clase, n), y = n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Distribución de especies amenazadas por clase",
    x = "Clase",
    y = "Número de especies"
  ) +
  theme_minimal()


library(dplyr)

datos <- datos %>%
  mutate(
    uicn_cat = case_when(
      Amenazada_Global_IUCN == "CR_IUCN" ~ "CR",
      Amenazada_Global_IUCN == "EN_IUCN" ~ "EN",
      Amenazada_Global_IUCN == "VU_IUCN" ~ "VU",
      Amenazada_Global_IUCN == "NT_IUCN" ~ "NT",
      Amenazada_Global_IUCN == "LC_IUCN" ~ "LC",
      Amenazada_Global_IUCN == "DD_IUCN" ~ "DD",
      TRUE ~ "No evaluada"
    )
  )

table(datos$uicn_cat)

library(ggplot2)

datos %>%
  count(Clase, uicn_cat) %>%
  ggplot(aes(x = Clase, y = n, fill = uicn_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Categorías UICN por clase taxonómica",
    x = "Clase",
    y = "Número de especies",
    fill = "UICN"
  ) +
  theme_minimal()


datos %>%
  count(uicn_cat) %>%
  ggplot(aes(x = reorder(uicn_cat, -n), y = n)) +
  geom_col() +
  labs(
    title = "Número de especies por categoría UICN",
    x = "Categoría UICN",
    y = "Número de especies"
  ) +
  theme_minimal()


datos %>%
  filter(uicn_cat %in% c("CR", "EN", "VU")) %>%
  count(uicn_cat) %>%
  ggplot(aes(x = uicn_cat, y = n)) +
  geom_col() +
  labs(
    title = "Especies amenazadas según UICN",
    x = "Categoría",
    y = "Número de especies"
  ) +
  theme_minimal()


datos <- datos %>%
  mutate(
    res_cat = case_when(
      Amenazada_Nacional_Res_0126 == "CR_MADS" ~ "CR",
      Amenazada_Nacional_Res_0126 == "EN_MADS" ~ "EN",
      Amenazada_Nacional_Res_0126 == "VU_MADS" ~ "VU",
      is.na(Amenazada_Nacional_Res_0126) ~ "No listada",
      TRUE ~ "No listada"
    )
  )

datos %>%
  count(Clase, res_cat) %>%
  ggplot(aes(x = Clase, y = n, fill = res_cat)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Categorías de amenaza según Resolución nacional (MADS)",
    x = "Clase",
    y = "Número de especies",
    fill = "Categoría"
  ) +
  theme_minimal()

datos %>%
  count(res_cat) %>%
  ggplot(aes(x = reorder(res_cat, -n), y = n)) +
  geom_col() +
  labs(
    title = "Número de especies según Resolución nacional (MADS)",
    x = "Categoría",
    y = "Número de especies"
  ) +
  theme_minimal()






# numeros -----------------------------------------------------------------


datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(endemica_cat) %>%
  ggplot(aes(x = endemica_cat, y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(
    title = "Especies amenazadas según endemismo",
    x = "",
    y = "Número de especies"
  ) +
  theme_minimal()



datos %>%
  filter(
    Amenazada_Global == "Amenazada",
    endemica_cat == "Endémica"
  ) %>%
  count(Clase, sort = TRUE) %>%
  ggplot(aes(x = reorder(Clase, n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Clases taxonómicas de especies amenazadas y endémicas",
    x = "Clase",
    y = "Número de especies"
  ) +
  theme_minimal()


datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(Clase) %>%
  ggplot(aes(x = reorder(Clase, n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), hjust = -0.1) +
  coord_flip() +
  labs(
    title = "Distribución de especies amenazadas por clase",
    x = "Clase",
    y = "Número de especies"
  ) +
  theme_minimal()

datos %>%
  count(Clase, uicn_cat) %>%
  ggplot(aes(x = Clase, y = n, fill = uicn_cat)) +
  geom_col() +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "Categorías UICN por clase taxonómica",
    x = "Clase",
    y = "Número de especies",
    fill = "UICN"
  ) +
  theme_minimal()


datos %>%
  count(uicn_cat) %>%
  ggplot(aes(x = reorder(uicn_cat, -n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(
    title = "Número de especies por categoría UICN",
    x = "Categoría UICN",
    y = "Número de especies"
  ) +
  theme_minimal()


datos %>%
  count(Clase, res_cat) %>%
  ggplot(aes(x = Clase, y = n, fill = res_cat)) +
  geom_col() +
  geom_text(
    aes(label = n),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  coord_flip() +
  labs(
    title = "Categorías de amenaza según Resolución nacional (MADS)",
    x = "Clase",
    y = "Número de especies",
    fill = "Categoría"
  ) +
  theme_minimal()

datos %>%
  count(res_cat) %>%
  ggplot(aes(x = reorder(res_cat, -n), y = n)) +
  geom_col() +
  geom_text(aes(label = n), vjust = -0.3) +
  labs(
    title = "Número de especies según Resolución nacional (MADS)",
    x = "Categoría",
    y = "Número de especies"
  ) +
  theme_minimal()



# proporciones ------------------------------------------------------------


library(tidyverse)
library(readr)

# Cargar datos
datos <- read_delim("Listado.csv", delim = ";")

# Transformación de datos
datos <- datos %>%
  mutate(
    # Variables de amenaza
    Amenazada_Global = if_else(
      Amenazada_Global_IUCN %in% c("CR_IUCN", "EN_IUCN", "VU_IUCN"),
      "Amenazada",
      "No amenazada"
    ),
    Amenazada_Nacional = if_else(
      Amenazada_Nacional_Res_0126 %in% c("CR", "EN", "VU"),
      "Amenazada",
      "No amenazada"
    ),
    # Endemismo
    endemica_cat = case_when(
      Endemica == "Endemica" ~ "Endémica",
      is.na(Endemica) ~ "No endémica",
      TRUE ~ "No endémica"
    ),
    # Categoría UICN actualizada (sin LC)
    uicn_cat = case_when(
      Amenazada_Global_IUCN == "CR_IUCN" ~ "CR",
      Amenazada_Global_IUCN == "EN_IUCN" ~ "EN",
      Amenazada_Global_IUCN == "VU_IUCN" ~ "VU",
      Amenazada_Global_IUCN == "NT_IUCN" ~ "NT",
      Amenazada_Global_IUCN == "LC_IUCN" ~ "LC",
      Amenazada_Global_IUCN == "DD_IUCN" ~ "DD",
      TRUE ~ "No evaluada"
    ),
    # Categoría para análisis excluyendo DD, No evaluada y LC
    uicn_cat_filtrada = if_else(
      uicn_cat %in% c("DD", "No evaluada", "LC"),
      NA_character_,
      uicn_cat
    ),
    # Categoría resolución nacional
    res_cat = case_when(
      Amenazada_Nacional_Res_0126 == "CR_MADS" ~ "CR",
      Amenazada_Nacional_Res_0126 == "EN_MADS" ~ "EN",
      Amenazada_Nacional_Res_0126 == "VU_MADS" ~ "VU",
      is.na(Amenazada_Nacional_Res_0126) ~ "No listada",
      TRUE ~ "No listada"
    )
  )

# 1. PROPORCIÓN de especies amenazadas según endemismo (sin DD/No eval/LC)
datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(endemica_cat, uicn_cat_filtrada) %>%
  group_by(endemica_cat) %>%
  mutate(
    total_grupo = sum(n),
    proporcion = n / total_grupo * 100
  ) %>%
  ggplot(aes(x = endemica_cat, y = proporcion, fill = uicn_cat_filtrada)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    position = position_stack(vjust = 0.5),
    size = 3
  ) +
  labs(
    title = "Proporción de categorías UICN por endemismo (excluyendo DD, LC y No evaluadas)",
    x = "Endemismo",
    y = "Porcentaje (%)",
    fill = "Categoría UICN"
  ) +
  theme_minimal()

# 2. PROPORCIÓN de clases taxonómicas para especies AMENAZADAS y ENDÉMICAS
datos %>%
  filter(
    Amenazada_Global == "Amenazada",
    endemica_cat == "Endémica",
    !is.na(uicn_cat_filtrada)
  ) %>%
  count(Clase) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    Clase = reorder(Clase, n)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "Proporción de clases taxonómicas - Especies amenazadas y endémicas",
    x = "Clase",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

# 3. PROPORCIÓN de especies amenazadas por clase (todas las amenazadas)
datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(Clase) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    Clase = reorder(Clase, n)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    hjust = -0.1
  ) +
  coord_flip() +
  labs(
    title = "Proporción de especies amenazadas por clase",
    x = "Clase",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

# 4. PROPORCIÓN de categorías UICN por clase (excluyendo DD, No eval, LC)
datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(Clase, uicn_cat_filtrada) %>%
  group_by(Clase) %>%
  mutate(
    total_clase = sum(n),
    proporcion = n / total_clase * 100
  ) %>%
  ggplot(aes(x = Clase, y = proporcion, fill = uicn_cat_filtrada)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = if_else(proporcion > 5, paste0(round(proporcion, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.8
  ) +
  coord_flip() +
  labs(
    title = "Proporción de categorías UICN por clase (excluyendo DD, LC y No evaluadas)",
    x = "Clase",
    y = "Porcentaje (%)",
    fill = "Categoría UICN"
  ) +
  theme_minimal()

# 5. DISTRIBUCIÓN de categorías UICN filtradas (solo CR, EN, VU, NT)
datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(uicn_cat_filtrada) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    uicn_cat_filtrada = factor(uicn_cat_filtrada, 
                               levels = c("CR", "EN", "VU", "NT"))
  ) %>%
  ggplot(aes(x = uicn_cat_filtrada, y = proporcion)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3
  ) +
  labs(
    title = "Distribución de categorías UICN (excluyendo DD, LC y No evaluadas)",
    x = "Categoría UICN",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

# 6. PROPORCIÓN de especies amenazadas (CR+EN+VU) según UICN
datos %>%
  filter(uicn_cat %in% c("CR", "EN", "VU")) %>%
  count(uicn_cat) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100
  ) %>%
  ggplot(aes(x = uicn_cat, y = proporcion)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3
  ) +
  labs(
    title = "Proporción de especies amenazadas según UICN",
    x = "Categoría de amenaza",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

# 7. PROPORCIÓN de categorías de resolución nacional por clase
datos %>%
  filter(res_cat != "No listada") %>%
  count(Clase, res_cat) %>%
  group_by(Clase) %>%
  mutate(
    total_clase = sum(n),
    proporcion = n / total_clase * 100
  ) %>%
  ggplot(aes(x = Clase, y = proporcion, fill = res_cat)) +
  geom_col(position = "stack") +
  geom_text(
    aes(label = if_else(proporcion > 5, paste0(round(proporcion, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.8
  ) +
  coord_flip() +
  labs(
    title = "Proporción de categorías de amenaza nacional (MADS) por clase",
    x = "Clase",
    y = "Porcentaje (%)",
    fill = "Categoría"
  ) +
  theme_minimal()

# 8. DISTRIBUCIÓN de categorías de resolución nacional
datos %>%
  filter(res_cat != "No listada") %>%
  count(res_cat) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100
  ) %>%
  ggplot(aes(x = reorder(res_cat, -proporcion), y = proporcion)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3
  ) +
  labs(
    title = "Distribución de categorías de amenaza nacional (MADS)",
    x = "Categoría",
    y = "Porcentaje (%)"
  ) +
  theme_minimal() +
  scale_y_continuous(limits = c(0, 100))

# Tabla resumen de exclusión
resumen_exclusion <- datos %>%
  summarise(
    Total_especies = n(),
    DD = sum(uicn_cat == "DD"),
    LC = sum(uicn_cat == "LC"),
    No_evaluada = sum(uicn_cat == "No evaluada"),
    Incluidas_analisis = sum(!is.na(uicn_cat_filtrada))
  ) %>%
  mutate(
    Porc_DD = DD / Total_especies * 100,
    Porc_LC = LC / Total_especies * 100,
    Porc_No_eval = No_evaluada / Total_especies * 100,
    Porc_incluidas = Incluidas_analisis / Total_especies * 100
  )

print(resumen_exclusion)




# Llenado de clase --------------------------------------------------------

df <- read_delim(
  "Listado_MM.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)

# 1. Limpieza inicial de texto
df <- df %>%
  mutate(
    Orden = str_trim(Orden),
    Clase = str_trim(Clase)
  )

# 2. Asignación completa de Clase por Orden
df <- df %>%
  mutate(
    Clase = case_when(
      # INSECTOS (Insecta)
      Orden %in% c("Lepidoptera", "Coleoptera", "Odonata", "Hymenoptera", 
                   "Diptera", "Hemiptera", "Trichoptera", "Ephemeroptera", 
                   "Plecoptera", "Orthoptera", "Blattodea", "Megaloptera") ~ "Insecta",
      
      # ARÁCNIDOS (Arachnida)
      Orden %in% c("Araneae", "Scorpiones", "Opiliones", "Acari") ~ "Arachnida",
      
      # PECES ÓSEOS (Actinopterygii)
      Orden %in% c("Siluriformes", "Characiformes", "Gymnotiformes", 
                   "Perciformes", "Cichliformes", "Cyprinodontiformes", 
                   "Synbranchiformes", "Atheriniformes", "Beloniformes") ~ "Actinopterygii",
      
      # PECES CARTILAGINOSOS (Chondrichthyes)
      Orden %in% c("Myliobatiformes", "Rhinopristiformes", "Squaliformes") ~ "Chondrichthyes",
      
      # MAMÍFEROS (Mammalia)
      Orden %in% c("Carnivora", "Cetartiodactyla", "Sirenia", "Rodentia", 
                   "Chiroptera", "Didelphimorphia", "Pilosa", "Cingulata", "Primates") ~ "Mammalia",
      
      # AVES (Aves)
      Orden %in% c("Passeriformes", "Anseriformes", "Pelecaniformes", "Charadriiformes", 
                   "Cathartiformes", "Accipitriformes", "Columbiformes", "Piciformes", 
                   "Psittaciformes", "Caprimulgiformes", "Strigiformes") ~ "Aves",
      
      # REPTILES (Reptilia)
      Orden %in% c("Squamata", "Testudines", "Crocodilia") ~ "Reptilia",
      
      # ANFIBIOS (Amphibia)
      Orden %in% c("Anura", "Caudata", "Gymnophiona") ~ "Amphibia",
      
      # MOLUSCOS (Gastropoda / Bivalvia)
      Orden %in% c("Architaenioglossa", "Hygrophila", "Unionida") ~ "Gastropoda",
      
      # Mantener valor previo si existe o NA si no hay coincidencia
      !is.na(Clase) & Clase != "" ~ Clase,
      TRUE ~ NA_character_
    )
  )

# Verificar qué valores quedaron en Clase y si aún hay NAs
table(df$Clase, useNA = "always")

# graficas ---------------------------------------------------------------

library(tidyverse)
library(readr)

datos <- read_delim(
  "Listado_MM.csv",
  delim = ";",
  locale = locale(encoding = "Latin1")
)

datos<-df

datos <- datos %>%
  mutate(
    # Variables de amenaza
    Amenazada_Global = if_else(
      Amenazada_Global_IUCN %in% c("CR_IUCN", "EN_IUCN", "VU_IUCN"),
      "Amenazada",
      "No amenazada"
    ),
    Amenazada_Nacional = if_else(
      Amenazada_Nacional_Res_0126 %in% c("CR", "EN", "VU"),
      "Amenazada",
      "No amenazada"
    ),
    # Endemismo
    endemica_cat = case_when(
      Endemica == "Endémica" ~ "Endémica",
      is.na(Endemica) ~ "No endémica",
      TRUE ~ "No endémica"
    ),
    # Categoría UICN actualizada (sin LC)
    uicn_cat = case_when(
      Amenazada_Global_IUCN == "CR_IUCN" ~ "CR",
      Amenazada_Global_IUCN == "EN_IUCN" ~ "EN",
      Amenazada_Global_IUCN == "VU_IUCN" ~ "VU",
      Amenazada_Global_IUCN == "NT_IUCN" ~ "NT",
      Amenazada_Global_IUCN == "LC_IUCN" ~ "LC",
      Amenazada_Global_IUCN == "DD_IUCN" ~ "DD",
      TRUE ~ "No evaluada"
    ),
    # Categoría para análisis excluyendo DD, No evaluada y LC
    uicn_cat_filtrada = if_else(
      uicn_cat %in% c("DD", "No evaluada", "LC"),
      NA_character_,
      uicn_cat
    ),
    # Categoría resolución nacional
    res_cat = case_when(
      Amenazada_Nacional_Res_0126 == "CR_MADS" ~ "CR",
      Amenazada_Nacional_Res_0126 == "EN_MADS" ~ "EN",
      Amenazada_Nacional_Res_0126 == "VU_MADS" ~ "VU",
      is.na(Amenazada_Nacional_Res_0126) ~ "No listada",
      TRUE ~ "No listada"
    )
  )


theme_custom <- theme_minimal() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_line(color = "grey90", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    axis.title = element_text(size = 10),
    axis.text = element_text(size = 9),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 8),
    legend.position = "right",
    plot.margin = margin(10, 10, 10, 10)
  )

colores_uicn <- c("CR" = "#D73027", "EN" = "#FC8D59", "VU" = "#FEE08B", "NT" = "#91CF60")
colores_res <- c("CR" = "#A50026", "EN" = "#F46D43", "VU" = "#FEE090")


p1 <- datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(endemica_cat, uicn_cat_filtrada) %>%
  group_by(endemica_cat) %>%
  mutate(
    total_grupo = sum(n),
    proporcion = n / total_grupo * 100
  ) %>%
  ggplot(aes(x = endemica_cat, y = proporcion, fill = uicn_cat_filtrada)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "black"
  ) +
  labs(
    x = "Endemismo",
    y = "Porcentaje (%)",
    fill = "Categoría UICN"
  ) +
  theme_custom +
  scale_fill_manual(values = colores_uicn) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))


p2 <- datos %>%
  filter(
    Amenazada_Global == "Amenazada",
    endemica_cat == "Endémica",
    !is.na(uicn_cat_filtrada)
  ) %>%
  count(Clase) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    Clase = fct_reorder(Clase, n)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion)) +
  geom_col(fill = "#3182BD", width = 0.7) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  labs(
    x = "Clase",
    y = "Porcentaje (%)"
  ) +
  theme_custom +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  )

p2

p3 <- datos %>%
  filter(Amenazada_Global == "Amenazada") %>%
  count(Clase) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    Clase = fct_reorder(Clase, n)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion)) +
  geom_col(fill = "#756BB1", width = 0.7) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    hjust = -0.1,
    size = 3
  ) +
  coord_flip() +
  labs(
    x = "Clase",
    y = "Porcentaje (%)"
  ) +
  theme_custom +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.05))
  )


p4 <- datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(Clase, uicn_cat_filtrada) %>%
  group_by(Clase) %>%
  mutate(
    total_clase = sum(n),
    proporcion = n / total_clase * 100,
    Clase = fct_reorder(Clase, total_clase)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion, fill = uicn_cat_filtrada)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = if_else(proporcion > 5, paste0(round(proporcion, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.5,
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "Clase",
    y = "Porcentaje (%)",
    fill = "Categoría UICN"
  ) +
  theme_custom +
  scale_fill_manual(values = colores_uicn) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

p4

p5 <- datos %>%
  filter(!is.na(uicn_cat_filtrada)) %>%
  count(uicn_cat_filtrada) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    uicn_cat_filtrada = factor(uicn_cat_filtrada,
                               levels = c("CR", "EN", "VU", "NT"))
  ) %>%
  ggplot(aes(x = uicn_cat_filtrada, y = proporcion)) +
  geom_col(fill = c("#D73027", "#FC8D59", "#FEE08B", "#91CF60"), width = 0.6) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    x = "Categoría UICN",
    y = "Porcentaje (%)"
  ) +
  theme_custom +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.1))
  )

p6 <- datos %>%
  filter(uicn_cat %in% c("CR", "EN", "VU")) %>%
  count(uicn_cat) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    uicn_cat = factor(uicn_cat, levels = c("CR", "EN", "VU"))
  ) %>%
  ggplot(aes(x = uicn_cat, y = proporcion)) +
  geom_col(fill = c("#A50026", "#F46D43", "#FEE090"), width = 0.6) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    x = "Categoría de amenaza",
    y = "Porcentaje (%)"
  ) +
  theme_custom +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.1))
  )

p7 <- datos %>%
  filter(res_cat != "No listada") %>%
  count(Clase, res_cat) %>%
  group_by(Clase) %>%
  mutate(
    total_clase = sum(n),
    proporcion = n / total_clase * 100,
    Clase = fct_reorder(Clase, total_clase)
  ) %>%
  ggplot(aes(x = Clase, y = proporcion, fill = res_cat)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    aes(label = if_else(proporcion > 5, paste0(round(proporcion, 1), "%"), "")),
    position = position_stack(vjust = 0.5),
    size = 2.5,
    color = "black"
  ) +
  coord_flip() +
  labs(
    x = "Clase",
    y = "Porcentaje (%)",
    fill = "Categoría"
  ) +
  theme_custom +
  scale_fill_manual(values = colores_res) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

p8 <- datos %>%
  filter(res_cat != "No listada") %>%
  count(res_cat) %>%
  mutate(
    total = sum(n),
    proporcion = n / total * 100,
    res_cat = factor(res_cat, levels = c("CR", "EN", "VU"))
  ) %>%
  ggplot(aes(x = res_cat, y = proporcion)) +
  geom_col(fill = c("#A50026", "#F46D43", "#FEE090"), width = 0.6) +
  geom_text(
    aes(label = paste0(round(proporcion, 1), "%\n(n=", n, ")")),
    vjust = -0.3,
    size = 3.5
  ) +
  labs(
    x = "Categoría",
    y = "Porcentaje (%)"
  ) +
  theme_custom +
  scale_y_continuous(
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0.1))
  )

ggsave("grafica1MM.png", p1, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("grafica2MM.png", p2, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("grafica3MM.png", p3, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("grafica4MM.png", p4, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("grafica5MM.png", p5, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("grafica6MM.png", p6, width = 8, height = 6, dpi = 300, bg = "white")
ggsave("grafica7MM.png", p7, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("grafica8MM.png", p8, width = 8, height = 6, dpi = 300, bg = "white")
