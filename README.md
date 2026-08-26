# STAR

STAR es una herramienta práctica y científicamente robusta, diseñada para traducir los ambiciosos objetivos del KMGBF en pasos accionables y medibles a diferentes escalas. Asimismo, STAR puede orientar metas y objetivos en otros Acuerdos Multilaterales Ambientales (MEAs), como la Convención Ramsar, la Convención sobre Especies Migratorias, la Convención de Patrimonio Mundial y la Convención de las Naciones Unidas de Lucha contra la Desertificación, además de contribuir al seguimiento de los Objetivos de Desarrollo Sostenible (ODS). Como métrica, facilita la toma de decisiones informadas, el seguimiento del progreso y la demostración del compromiso gubernamental para detener y revertir la pérdida de biodiversidad.
STAR significa Species Threat Abatement and Restoration (Reducción de Amenazas a las Especies y Restauración). Se trata de una métrica global de biodiversidad derivada de la Lista Roja de Especies Amenazadas de la UICN, calculada mediante un procedimiento estandarizado basado en datos espacialmente explícitos. Su enfoque integra información sobre la presencia actual e histórica de especies amenazadas y casi amenazadas, las amenazas que enfrentan y su riesgo de extinción, para producir dos capas globales complementarias:
STAR-T, enfocada en la reducción de amenazas, y
STAR-R, enfocada en restauración.
La metodología STAR genera puntajes que, para cualquier área de interés, indican la contribución potencial de acciones de manejo —ya sea reducción de amenazas o restauración— a la disminución del riesgo global de extinción de especies. Los puntajes pueden desagregarse por tipos específicos de amenaza, utilizando la información de la Lista Roja sobre la intensidad y alcance de las presiones que afectan a cada especie. Esto permite identificar acciones focalizadas, compararlas entre sí y evaluar su contribución relativa a la reducción del riesgo de extinción.
Los puntajes STAR son aditivos, comparables y escalables entre amenazas y geografías, lo que convierte a STAR en una métrica versátil y consistente para la planificación, la priorización y la evaluación de resultados en conservación.


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
  <img src="docs/assets/img/Metodologia%20STAR.png" width="600">
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
