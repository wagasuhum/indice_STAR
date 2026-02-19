#  Propósito del STAR 

El propósito principal de la métrica STAR (Species Threat Abatement and Restoration) es cuantificar la contribución potencial de las acciones orientadas a mitigar amenazas y restaurar hábitats en la reducción del riesgo de extinción de especies a nivel global (cita). Esta métrica fue desarrollada para hacer explícitas espacialmente las acciones necesarias para detener y revertir la pérdida de especies (cita).

---

## ☘️ Qué hace

- Cuantificación de la contribución a la reducción del riesgo de extinción
- Identificación espacialmente explícita
- Apoyo a metas basadas en la ciencia
- Apoyo a la implementación del Marco Global de Biodiversidad
- Involucramiento de diversos actores
- Complemento a herramientas existentes
- Escalabilidad y versatilidad
- Adaptabilidad a diferentes tipos de datos
- Apoyo a la planificación de la conservación (pero no como herramienta única de priorización)
- Seguimiento del progreso

## 🌳Insumos

- Lista Roja de Especies Amenazadas de la UICN
- Base de Datos Mundial de Áreas Clave para la Biodiversidad (WDKBA)
- Datos de Cobertura del Suelo
- Mapas de Cambio en la Cobertura Forestal
- Modelos Digitales de Elevación (DEM)
- Listas Rojas Nacionales

---

## 🧭Limitaciones

###Limitaciones intrínsecas de STAR

- Enfoque en especies amenazadas o casi amenazadas
- Suposición de eliminación total de amenazas
- Simplificación de la restauración
- Consideraciones sobre la escala espacial
- Reflejo limitado de la complejidad de las amenazas
- Amenazas globales

###Limitaciones relacionadas con los datos subyacentes

- Enfoque en especies amenazadas o casi amenazadas
- Variabilidad en las listas rojas nacionales:


## 🧮 Índice STAR(T)

$$
STAR(T) =
\sum_{s}
\sum_{i}
N_s
P_{s,i}
W_s
C_{s,t}
$$

Donde:

$$
\begin{aligned}
N_s &= \text{Número de especies } s \text{ presentes en la unidad espacial} \\
P_{s,i} &= \text{Proporción del área de hábitat (AOH) de la especie } s \\
        &\quad \text{presente en la ubicación } i \\
W_s &= \text{Peso asociado al nivel de amenaza de la especie } s \\
C_{s,t} &= \text{Contribución de la amenaza } t \\
        &\quad \text{al riesgo de extinción de la especie } s
\end{aligned}
$$

## ⭐ Cálculo de STAR (R)

$$
STAR(R) =
\sum_{s}
\sum_{i}
N_s
P_{s,i}
W_s
R_{s}
$$

Donde:


$$
\begin{aligned}
N_s &= \text{Número de especies } s \text{ presentes en la unidad espacial} \\
P_{s,i} &= \text{Proporción del área de hábitat (AOH) potencialmente recuperable de la especie } s \\
        &\quad \text{en la ubicación } i \\
W_s &= \text{Peso asociado al nivel de amenaza de la especie } s \\
R_s &= \text{Potencial de recuperación de la especie } s \\
     &\quad \text{mediante acciones de restauración del hábitat}
\end{aligned}
$$


## Especies que se encuentran amenazadas segun IUCN

Para las 5735 especies reportadas en la zona del piedemonte de Casanare, un total de 62 especies son endémicas y se encuentran en algún grado de amenaza según la UICN, un 74.2% (46) de las especies se encuentran en peligro y vulnerable, mientras que un 14.5% de taxones se encuentra en casi amenazados y 11.3% en peligro crítico. En contraste, para la zona las especies no endémicas se distribuyen en las categorías de casi amenazadas con 45.6%, en vulnerables con un 41.2%, y un 13.2% en peligro.

<p align="center">
  <img src="assets/img/grafica1.png" width="600">
</p>

La mayoría de especies amenazadas y que son endémicas pertenece a las plantas que comprenden las clases Magnoliopsida (55.6%), Liliopsida (7.5%), y Cicadopsida (1.9%), para el reino Animalia, la mayoría corresponde a anfibios (15.1%), seguido por los peces de la clase Actinopterygii (9.4%), las aves (7.5%) y por ultimo, los mamíferos (1.9%).

<p align="center">
  <img src="assets/img/grafica2.png" width="600">
</p>


```bash
pip install habitat-tool

## 🚀 Nueva sección
Contenido de prueba

