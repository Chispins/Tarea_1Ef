# Tarea_1Ef
## Integrantes:
- Sergio Contreras
- Ricardo Villalobos

## Ejecución Pregunta 1 ( Modelo CAPM)
La Pregunta 1 está desarrollada en R y requiere la instalación previa de ciertas librerías.

### Requisitos
Tener instalado:
- R versión 4.0 o superior
- Conexión a internet para descargar datos desde Yahoo Finance

### Librerías usadas
Ejecutar en R una sola vez:
install.packages(c("quantmod", "PerformanceAnalytics", "lubridate", "dplyr", "tidyr", "ggplot2"))

## Ejecución
- Abrir R, RStudio o PyCharm con soporte R
- Ubicarse en la carpeta del proyecto
- Ejecutar:
  source("Pregunta_1.r")
  
## Salidas generadas
El script genera automáticamente:
- graficos_excesos.jpg: Series de excesos de retorno
- evolucion_alpha.jpg: Evolución anual del alpha
- evolucion_beta.jpg: Evolución anual del beta
Tambien imprime:
- Estadística descriptiva
- Resultados de regresiones CAPM
- Test de hipótesis para beta
- 
## Notas importantes
- Los datos se descargan automáticamente desde Yahoo Finance
- Se utilizan datos semanales de los últimos 10 años
- Los resultados pueden variar levemente dependiendo de la fecha de ejecución
- Si aparecen errores gráficos, ejecutar:
  graphics.off() o reiniciar la sesión de R

Código del Grupo X para la Tarea de Econometría Financiera `bbdd_Tarea1.xlsx`.

## Estructura esperada

Los siguientes archivos deben estar en la carpeta del proyecto:

- `Pregunta_2.py`
- `bbdd_Tarea1.xlsx`
- `requirements.txt`

## Instalacion

1. Abre una terminal en la carpeta del proyecto.
2. Instala dependencias:

```powershell
pip install -r requirements.txt
```

## Problemas comunes

- `ModuleNotFoundError`: vuelve a instalar dependencias con `pip install -r requirements.txt`.
- `FileNotFoundError` para `bbdd_Tarea1.xlsx`: verifica que el archivo este en la misma carpeta que `Pregunta_2.py`.

