# Se cargan las librerías
# Ecosistema Financiero
library(quantmod)            # Se usa para la gestión de datos financieros, descarga de precios y modelado.
library(PerformanceAnalytics) # Se usa para el cálculo de métricas de rendimiento y riesgo.
library(lubridate)            # Se usa para la manipulación eficiente de formatos de fechas y tiempos.
# Ecosistema Tidyverse
library(dplyr)                # Se usa para la manipulación y transformación de datos (filtrar, agrupar, resumir).
library(tidyr)                # Se usa para la limpieza y estructuración de datos para que sean legibles (formato tidy).
library(ggplot2)              # Se usa para la creación de gráficos profesionales y visualización de datos.

# PARTE A: Obtenga las series de exceso de retorno del mercado y del activo i. Realice estadística descriptiva y
# grafique estos excesos para cada empresa (4 gráficos en total: 3 correspondientes a los excesos de retorno de los activos
# y 1 al exceso de retorno del mercado). Explique sus principales hallazgos. (Ocupe datos semanales de los ultimos 10 años)

# Se configuran las fechas y activos
fecha_fin <- Sys.Date()                             # Se obtiene la fecha actual del sistema
fecha_inicio <- fecha_fin - years(10)               # Se resta 10 años a la fecha actual usando lubridate
tickers <- c("AAPL", "MSFT", "AMZN", "^GSPC", "^IRX") # Se definen activos, S&P500 y T-Bill

# Se descargan los datos desde Yahoo Finance
# periodicity = "weekly" para descargar los datos con frecuencia semanal
getSymbols(tickers, src = "yahoo", from = fecha_inicio, to = fecha_fin, periodicity = "weekly")

# Se preparan los precios ajustados
# Ad() extrae el precio ajustado (por dividendos y splits) de cada objeto descargado
precios <- merge(Ad(AAPL), Ad(MSFT), Ad(AMZN), Ad(GSPC), Ad(IRX))
colnames(precios) <- c("AAPL", "MSFT", "AMZN", "Mercado", "IRX") # Se renombraron las columnas
precios <- na.omit(precios)                                      # Se eliminan las filas con valores faltantes

# Se cálcularon los retornos y tasa Libre de riesgo (RF)
retornos <- Return.calculate(precios[, 1:4], method = "log")     # Se calculan los retornos logarítmicos
# Se convierte la tasa anual del tesoro (IRX) a una tasa efectiva semanal
rf_semanal <- (1 + precios$IRX/100)^(1/52) - 1
colnames(rf_semanal) <- "RF"

# Se consolidan los retornos y se hace una limpieza final
retornos <- merge(retornos, rf_semanal)
retornos <- na.omit(retornos)

# Se calculan los excesos de retorno
# Se resta la tasa RF a cada activo (R_i - R_f) para obtener el premio por riesgo
series_excesos <- sweep(retornos[, 1:4], 1, retornos$RF, FUN = "-")
colnames(series_excesos) <- c("AAPL_ex", "MSFT_ex", "AMZN_ex", "Mercado_ex")

# Se hace la "Estadística descriptiva"
cat(" Estadística descriptiva de los excesos de retorno \n")
# Se genera tabla con media, desviación estándar, skewness, kurtosis, etc.
print(table.Stats(series_excesos))

# Se generan los gráficos de serie de tiempo
graphics.off() # # Cierra cualquier dispositivo gráfico previo para evitar conflictos de memoria

# Se exportan los gráficos a un archivo local en .jpg
jpeg("graficos_excesos.jpg", width = 1200, height = 900, res = 120)

par(mfrow = c(2, 2)) # Divide el lienzo en una cuadrícula de 2 filas y 2 columnas

# Se grafica cada serie individualmente usando zoo para manejo de fechas en el eje X
plot.zoo(series_excesos$AAPL_ex, main = "1. Exceso de retorno: AAPL", ylab = "Exceso", xlab = "Fecha", col = "blue", lwd = 1)
plot.zoo(series_excesos$MSFT_ex, main = "2. Exceso de retorno: MSFT", ylab = "Exceso", xlab = "Fecha", col = "darkgreen", lwd = 1)
plot.zoo(series_excesos$AMZN_ex, main = "3. Exceso de retorno: AMZN", ylab = "Exceso", xlab = "Fecha", col = "purple", lwd = 1)
plot.zoo(series_excesos$Mercado_ex, main = "4. Exceso de retorno: Mercado (S&P 500)", ylab = "Exceso", xlab = "Fecha", col = "red", lwd = 1)

par(mfrow = c(1, 1)) # Se restablece el lienzo a un solo gráfico
dev.off()            # Se finaliza la escritura del archivo y lo guarda

# PARTE B: Utilizando las series obtenidas anteriormente, estime los parámetros α y β para cada empresa e
# interprete los coeficientes.

# Convertir el objeto de series temporales (xts/zoo) a un data frame estándar para facilitar el uso de lm()
df_excesos <- as.data.frame(series_excesos)

# Regresión para AAPL
cat("\n Resultados regresión AAPL \n")
# lm() ajusta un modelo lineal: el exceso del activo es explicado por el exceso del mercado
modelo_aapl <- lm(AAPL_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_aapl)) # Muestra R-cuadrado, errores estándar y significancia estadística

# Regresión para MSFT
cat("\n Resultados regresión MSFT \n")
modelo_msft <- lm(MSFT_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_msft))

# Regresión para AMZN
cat("\n Resultados regresión AMZN \n")
modelo_amzn <- lm(AMZN_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_amzn))

# Se crea un data frame para consolidar los resultados clave de los tres modelos
tabla_coeficientes <- data.frame(
  Activo = c("AAPL", "MSFT", "AMZN"),

  # coef(...)[1] extrae el Intercepto (Alpha de Jensen): mide el valor extra generado
  Alpha  = c(coef(modelo_aapl)[1], coef(modelo_msft)[1], coef(modelo_amzn)[1]),

  # coef(...)[2] extrae la pendiente (Beta): mide la sensibilidad del activo ante el mercado
  Beta = c(coef(modelo_aapl)[2], coef(modelo_msft)[2], coef(modelo_amzn)[2])
)

# Se limpian los nombres de las filas para una presentación más estética
rownames(tabla_coeficientes) <- NULL

# Se imprime el resumen final
cat("\n Resumen de coeficientes estimados \n")
print(tabla_coeficientes)

# PARTE C: Con el fin de analizar la evolución temporal de α y β, calcule estos parámetros
# para cada año y para cada stock. Luego, grafique su evolución en el tiempo e interprete los resultados.

# Se crea un data frame dinámico uniendo las fechas (index) con los datos numéricos
df_dinamico <- data.frame(Fecha = index(series_excesos), coredata(series_excesos))
# Se extraen el año de cada observación para realizar agrupaciones anuales
df_dinamico$Year <- year(df_dinamico$Fecha)

# Se transforman los datos de formato "ancho" a formato "largo" para facilitar el uso de ggplot2 y dplyr
df_largo <- df_dinamico %>%
  pivot_longer(cols = c("AAPL_ex", "MSFT_ex", "AMZN_ex"),
               names_to = "Activo",
               values_to = "Exceso_Activo")

# Se calculan los coeficientes alpha y beta agrupados por año y por cada activo
resultados_anuales <- df_largo %>%
  group_by(Year, Activo) %>%
  summarise(
    Alpha = coef(lm(Exceso_Activo ~ Mercado_ex))[1], # Intercepto de la regresión anual
    Beta  = coef(lm(Exceso_Activo ~ Mercado_ex))[2], # Pendiente (beta) de la regresión anual
    .groups = "drop" # Desagrupa al finalizar para evitar problemas en cálculos futuros
  )

# Se resetea la ventana de gráficos para limpiar configuraciones previas
graphics.off()

# Se genera el gráfico de evolución del alpha
grafico_alpha <- ggplot(resultados_anuales, aes(x = Year, y = Alpha, color = Activo)) +
  geom_line(linewidth = 1) +                  # Línea de tendencia temporal
  geom_point(size = 2) +                      # Puntos para marcar cada año
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Referencia de Alpha = 0
  facet_wrap(~Activo, ncol = 1) +             # Separar un gráfico por cada empresa (verticalmente)
  theme_minimal() +                           # Estilo visual limpio y moderno
  labs(title = "Evolución temporal del alpha por año",
       x = "Año", y = "Alpha estimado") +
  theme(legend.position = "none")             # Ocultar leyenda (el título de la faceta ya indica el activo)

# Se guarda el gráfico de alpha
ggsave("evolucion_alpha.jpg", plot = grafico_alpha, width = 8, height = 6)

# Se genera el gráfico de evolución del beta
grafico_beta <- ggplot(resultados_anuales, aes(x = Year, y = Beta, color = Activo)) +
  geom_line(linewidth = 1) +                  # Línea de tendencia temporal
  geom_point(size = 2) +                      # Puntos para marcar cada año
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + # Referencia de Beta = 1 (Riesgo Mercado)
  facet_wrap(~Activo, ncol = 1) +             # Separar un gráfico por cada empresa
  theme_minimal() +
  labs(title = "Evolución temporal del beta por año",
       x = "Año", y = "Beta estimado") +
  theme(legend.position = "none")

# Guardar el gráfico de Beta en la carpeta de trabajo
ggsave("evolucion_beta.jpg", plot = grafico_beta, width = 8, height = 6)

# PARTE D: Testee la hipótesis nula β = 0 para cada empresa (los encontrados en el ítem b).
# Interprete sus resultados y relaciónelos con la teoría financiera subyacente al modelo CAPM.

# Se define una función personalizada para automatizar el test de cada activo
test_hipotesis_beta <- function(modelo, nombre_activo) {

  # Se extrae la matriz de coeficientes del resumen del modelo lineal
  coeficientes <- summary(modelo)$coefficients

  # Se obtiene la estimación puntual del beta (pendiente del modelo)
  estimacion_beta <- coeficientes[2, "Estimate"]

  # Se obtienen el estadístico t (mide cuántas desviaciones estándar se aleja el beta de cero)
  estadistico_t <- coeficientes[2, "t value"]

  # Se obtiene el p-valor (probabilidad de observar este beta si la hipótesis nula fuera cierta)
  p_valor <- coeficientes[2, "Pr(>|t|)"]

  # Se imprime el encabezado y planteamiento de las hipótesis estadísticas
  cat(sprintf("\n Test de Hipótesis para %s \n", nombre_activo))
  cat(sprintf("H0: Beta = 0  vs  H1: Beta != 0\n")) # Hipótesis nula vs alternativa

  # Se muestran los resultados numéricos formateados con 4 decimales y notación científica para el p-valor
  cat(sprintf("Beta estimado: %.4f\n", estimacion_beta))
  cat(sprintf("Estadístico t: %.4f\n", estadistico_t))
  cat(sprintf("P-valor:       %e\n", p_valor))

  # Se establece la lógica de decisión estadística basada en el nivel de significancia del 5% (alfa = 0.05)
  if(p_valor < 0.05) {
    cat("Conclusión: Se RECHAZA la hipótesis nula (H0) al 5% de significancia.\n")
    cat("Interpretación: El activo tiene una relación estadísticamente significativa con el mercado.\n")
  } else {
    cat("Conclusión: NO SE RECHAZA la hipótesis nula (H0) al 5% de significancia.\n")
    cat("Interpretación: No hay evidencia suficiente para decir que el activo se mueve con el mercado.\n")
  }
}

# Se ejecutan la función para cada uno de los modelos estimados previamente
test_hipotesis_beta(modelo_aapl, "Apple (AAPL)")
test_hipotesis_beta(modelo_msft, "Microsoft (MSFT)")
test_hipotesis_beta(modelo_amzn, "Amazon (AMZN)")

