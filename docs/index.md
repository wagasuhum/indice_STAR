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

## 🧮 Índice STAR(T)

$$
STAR(T) =
\left(
\sum_{s} N_s P_s
\right)
\cdot
\left(
\sum_{i} W_i C_i
\right)
$$

Donde:

$$
\begin{aligned}
N_s &= \text{Número de elementos del tipo } s \\
P_s &= \text{Peso o importancia del elemento } s \\
W_i &= \text{Peso del impacto } i \\
C_i &= \text{Magnitud o contribución del impacto } i
\end{aligned}
$$


## 🚀 Instalación

## Especies que se encuentran amenazadas segun IUCN

<p align="center">
  <img src="assets/img/grafica1.png" width="600">
</p>



```bash
pip install habitat-tool

## 🚀 Nueva sección
Contenido de prueba

