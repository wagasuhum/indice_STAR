# Descripción de Archivos Adicionales para el Indicador STAR

Documentación y enlaces a los scripts principales para el procesamiento de datos, cálculo del indicador STAR y análisis de presiones espaciales.

---

## 1. Recorte de Biomodelos con Área de Interés & Obtención de AOH

Para el cálculo del indicador **STAR**, se requiere recortar el área de interés para optimizar los tiempos de cómputo y enfocar el análisis. Asimismo, es necesario identificar el hábitat óptimo o **Área de Hábitat (AOH)** para cada especie incluida en los biomodelos a partir de sus parámetros iniciales.

* 📄 **Script:** [`Generacion de AOH.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Generacion%20de%20AOH.R)

---

## 2. Cálculo del Indicador STAR

Script principal para ejecutar el cálculo del indicador STAR utilizando los insumos previamente generados y obteniendo los archivos resultantes para su posterior inclusión en informes.

* 📄 **Script:** [`STAR.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/STAR.R)

---

## 3. Tablero de Control (Dashboard Interactive)

Tablero interactivo desarrollado en **Shiny** que permite la carga de polígonos personalizados para realizar el cálculo del indicador en los departamentos de **Casanare** y **Boyacá**.

> 💡 **Nota:** Actualmente el tablero no se encuentra alojado en la web debido a labores de mantenimiento, pero el código es totalmente funcional.

* 📄 **Script:** [`Shiny2.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Shiny2.R)

---

## 4. Visualización de Datos y Gráficas

Generación de analítica visual a partir de los listados del SiB para los núcleos de estudio. Permite cuantificar registros por categoría de amenaza (UICN) y estatus de endemicidad.

* 📄 **Script:** [`Graficas.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Graficas.R)

---

## 5. Riqueza Ponderada por Tesela

Para priorizar las unidades de análisis (teselas) con mayor valor de conservación, se calculó la riqueza ponderada cruzando las Áreas de Hábitat (AOH) con las categorías de la Lista Roja de la UICN. 

Los valores asignados por categoría siguen la lógica del indicador STAR:
* **LC** (*Preocupación Menor*): $0$
* **NT** (*Casi Amenazado*): $100$
* **VU** (*Vulnerable*): $200$
* **EN** (*En Peligro*): $300$
* **CR** (*En Peligro Crítico*): $400$

### Scripts según zona de estudio:
* 📍 **Casanare:** [`Riqueza ponderda.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Riqueza%20ponderda.R)
* 📍 **Magdalena Medio:** [`Riqueza_pondera_MM.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Riqueza_pondera_MM.R)

---

## 6. Presiones Espaciales por Tesela

Evaluación del impacto de actividades humanas sobre especies amenazadas. Se incorporaron capas espaciales de **fragmentación del paisaje**, **uso del suelo**, **densidad poblacional** e **infraestructura vial** (insumos del Índice de Huella Humana).

**Metodología:**
1. Reproyección y ajuste de resolución espacial a las capas AOH.
2. Normalización de variables mediante escalamiento Min-Max (rango $0$ a $1$).
3. Extracción de valores de presión dentro del hábitat disponible de cada especie.
4. Cálculo de presiones medias y ponderadas por STAR.

* 📄 **Script:** [`Presiones_MM.R`](https://github.com/wagasuhum/indice_STAR/blob/main/codes/Presiones_MM.R)
