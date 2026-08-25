#  Propósito del STAR 

El propósito principal de la métrica STAR (Species Threat Abatement and Restoration) es cuantificar la contribución potencial de las acciones orientadas a mitigar amenazas y restaurar hábitats en la reducción del riesgo de extinción de especies a nivel global  Mair et al. (2021). Esta métrica fue desarrollada para hacer explícitas espacialmente las acciones necesarias para detener y revertir la pérdida de especies  Mair et al. (2021).

---

## Qué hace

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

## Insumos

- Lista Roja de Especies Amenazadas de la UICN
- Base de Datos Mundial de Áreas Clave para la Biodiversidad (WDKBA)
- Datos de Cobertura del Suelo
- Mapas de Cambio en la Cobertura Forestal
- Modelos Digitales de Elevación (DEM)
- Listas Rojas Nacionales

## Flujo de trabajo para el calculo

<p align="center">
  <img src="assets/img/Metodologia STAR.png" width="600">
</p>


---

## Limitaciones

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


## Índice STAR(T)

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

## Cálculo de STAR (R)

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



##🦎 Poligonos de especies amenazadas segun categoria UICN

<p align="center">
  <img src="assets/img/grafica9.png" width="600">
</p>

### Poligonos de especies amenazadas por grupo

<p align="center">
  <img src="assets/img/grafica10.png" width="600">
</p>

##🦜 Poligonos de especies amenazdas segun categoria de resolucion 126 de 2024

<p align="center">
  <img src="assets/img/grafica11.png" width="600">
</p>

### Poligonos de especies amenazadas por grupo segun categoria de resolucion 126 de 2024

<p align="center">

### Amenazas segun la uicn para la especies presentes en la zona

  <img src="assets/img/grafica13.png" width="600">
</p>


## 🦦 Amenazas UICN para las especies presentes en el poligono

Un total de 42 especies para la zona del altiplano de Casanare presentan una amenaza en común de nivel uno que se relaciona con aspectos de desarrollo residencial y comercial, dentro de los cuales destaca el establecimiento de diferente infraestructura para la formación de asentamientos humanos, dentro de las cuales cinco especies (Chaetostoma dorsale, Chaetostoma formosae, Dolichancistrus fuesslii, Lontra longicaudis, Pentagonia magnifica), tiene una alcance major de este tipo de presión, lo que afectan notoriamente su categorización dentro de la lista roja; igualmente para el segundo nivel que corresponde a afectaciones en las especies generadas por actividades como agricultura y acuicultura específicamente actividades con cultivos anuales y perennes que tienen un impacto mayor en grupos de mamíferos como (Lontra longicaudis, Tremarctos ornatus, Myrmecophaga tridactyla).

El nivel tres de amenazas que corresponde a impactos relacionados con la minería y la producción de energía, dentro de estos destacan para la zona de interés las actividades petroleras, siendo la especie que presentan mayor afectación Tremarctos ornatus y en menor medida Panthera onca, Symplocos trianae, Farlowella acus, Creagrutus atratus, Farlowella colombiensis, Chaetostoma joropo, Bromelia trianae, Knodus meridae, mientras para el nivel cuatro relacionado con transporte y corredores de servicios, la infraestructura vial genera un impacto sobre todas las poblaciones de Ayenia stipularis.

<div style="margin-top:20px;">
  <a href="tabla_iucn.html" 
     style="background:#2c7a7b; color:white; padding:10px 18px; 
            text-decoration:none; border-radius:6px;">
     🔎 Consultar base de datos completa
  </a>
</div>



