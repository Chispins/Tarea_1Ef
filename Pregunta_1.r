# Tarea 1 - Econometría financiera (Pregunta 1)

# Se cargan las librerías
# Ecosistema financiero
library(quantmod) # Descarga de precios históricos y manejo de datos financieros.
library(PerformanceAnalytics) # Cálculo de estadísticas descriptivas y métricas de riesgo.
# Ecosistema Tidyverse
library(lubridate) # Manipulación rápida de fechas (ej. restar años o extraer meses).
library(dplyr) # Transformación de datos (filtrar, crear columnas, agrupar).
library(tidyr) # Reestructuración de tablas (pivotar de formato ancho a largo y viceversa).
library(ggplot2)  # Creación de gráficos estéticos y por capas.

# PARTE A: Obtenga las series de exceso de retorno del mercado y del activo i. Realice estadística descriptiva y
# grafique estos excesos para cada empresa (4 gráficos en total: 3 correspondientes a los excesos de retorno de los activos
# y 1 al exceso de retorno del mercado). Explique sus principales hallazgos. (Ocupe datos semanales de los ultimos 10 años)

fecha_fin <- as.Date("2026-04-01") # Establece la fecha de corte actual.
fecha_inicio <- fecha_fin - years(11) # Resta 11 años para tener 10 años de análisis + 1 año base para retornos.
tickers <- c("AAPL", "MSFT", "AMZN", "^GSPC", "^IRX") # Símbolos de Yahoo Finance (Activos, S&P500 y T-Bill).

# Se descargan los datos "diarios"
getSymbols(tickers, src = "yahoo", from = fecha_inicio, to = fecha_fin, periodicity = "daily")

# Se preparan los precios ajustados
precios <- merge(Ad(AAPL), Ad(MSFT), Ad(AMZN), Ad(GSPC), Ad(IRX)) # Une solo la columna de "precios ajustados" de cada activo.
colnames(precios) <- c("AAPL", "MSFT", "AMZN", "Mercado", "IRX") # Simplifica los nombres de las columnas.
precios <- na.omit(precios) # Limpia la base eliminando días sin cotización (feriados).

# Se transforman a un formato más amigable
df_precios <- data.frame(date = index(precios), coredata(precios)) %>% # Pasa de objeto temporal (xts) a tabla normal (data.frame).
  pivot_longer(cols = -date, names_to = "Activo", values_to = "Precio") # Apila los activos para facilitar cálculos en grupo.

# Calcular retornos comparando con la fecha más cercana del año anterior
df_retornos <- df_precios %>%
  mutate(fecha_ano_anterior = date - years(1)) %>% # Crea referencia temporal de t-1 año.
  left_join(
    df_precios,
    by = join_by(Activo, closest(fecha_ano_anterior >= date)), # Cruza la base buscando el precio del año pasado (o el día hábil más cercano).
    suffix = c("", "_prev") # Distingue entre el precio de hoy y el del año pasado.
  ) %>%
  mutate(
    retorno = case_when(
      Activo == "IRX" ~ Precio / 100, # La tasa libre de riesgo ya viene anualizada, solo se pasa a decimal.
      TRUE ~ suppressWarnings(log(Precio / Precio_prev)) # Para las acciones, calcula el retorno logarítmico interanual.
    )
  ) %>%
  filter(date >= (fecha_fin - years(10))) # Recorta el año base extra para dejar exactamente 10 años de estudio.

# Se pasa a formato ancho y se filtra para obtener datos "semanales"
df_excesos_semanales <- df_retornos %>%
  select(date, Activo, retorno) %>%
  pivot_wider(names_from = Activo, values_from = retorno) %>% # Desapila los activos de vuelta a columnas individuales.
  drop_na() %>%
  group_by(anio = year(date), semana = isoweek(date)) %>% # Agrupa los datos diariamente por la semana a la que pertenecen.
  slice_max(date, n = 1, with_ties = FALSE) %>% # Extrae únicamente el último día hábil de cada semana.
  ungroup() %>%
  mutate(
    # Calcula el "premio por riesgo" (Exceso) restando la tasa libre de riesgo (IRX) a cada retorno.
    AAPL_ex = AAPL - IRX,
    MSFT_ex = MSFT - IRX,
    AMZN_ex = AMZN - IRX,
    Mercado_ex = Mercado - IRX
  ) %>%
  select(date, AAPL_ex, MSFT_ex, AMZN_ex, Mercado_ex) # Mantiene solo las columnas de exceso.

# Se vuelve a convertir a formato de serie de tiempo (xts)
series_excesos <- xts(df_excesos_semanales[,-1], order.by = df_excesos_semanales$date) # Requisito de la librería PerformanceAnalytics.

# Estadística descriptiva
cat("\n Descriptive Statistics of Excess Returns \n")
print(table.Stats(series_excesos)) # Genera automáticamente media, varianza, asimetría, etc.

# Se generan los gráficos de serie de tiempo
graphics.off() # Limpia la memoria gráfica de R.
jpeg("graficos_excesos.jpg", width = 1200, height = 900, res = 120) # Guarda la imagen en alta calidad.

par(mfrow = c(2, 2)) # Divide el lienzo en una cuadrícula de 2x2.
plot.zoo(series_excesos$AAPL_ex, main = "1. Excess return: AAPL", ylab = "Excess", xlab = "Date", col = "blue", lwd = 1)
plot.zoo(series_excesos$MSFT_ex, main = "2. Excess return: MSFT", ylab = "Excess", xlab = "Date", col = "darkgreen", lwd = 1)
plot.zoo(series_excesos$AMZN_ex, main = "3. Excess return: AMZN", ylab = "Excess", xlab = "Date", col = "purple", lwd = 1)
plot.zoo(series_excesos$Mercado_ex, main = "4. Excess return: Market (S&P 500)", ylab = "Excess", xlab = "Date", col = "red", lwd = 1)

par(mfrow = c(1, 1)) # Restaura el lienzo a formato único.
dev.off() # Guarda el archivo JPG en la carpeta.

# PARTE B: Utilizando las series obtenidas anteriormente, estime los parámetros alfa y beta
# para cada empresa e interprete los coeficientes.

df_excesos <- as.data.frame(series_excesos) # Se regresa al formato data.frame para correr regresiones estándar.

# Estimación MCO (lm): El exceso del activo depende (~) del exceso del mercado.
cat("\n Regression results: AAPL \n")
modelo_aapl <- lm(AAPL_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_aapl))

cat("\n Regression results: MSFT \n")
modelo_msft <- lm(MSFT_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_msft))

cat("\n Regression results: AMZN \n")
modelo_amzn <- lm(AMZN_ex ~ Mercado_ex, data = df_excesos)
print(summary(modelo_amzn))

# Se consolida la información clave en una tabla resumen.
tabla_coeficientes <- data.frame(
  Asset = c("AAPL", "MSFT", "AMZN"),
  Alpha = c(coef(modelo_aapl)[1], coef(modelo_msft)[1], coef(modelo_amzn)[1]), # Extrae el Intercepto.
  Beta  = c(coef(modelo_aapl)[2], coef(modelo_msft)[2], coef(modelo_amzn)[2])  # Extrae la pendiente.
)

rownames(tabla_coeficientes) <- NULL # Limpia los nombres de las filas por estética.

cat("\n Summary of estimated coefficients \n")
print(tabla_coeficientes)

# PARTE C: Con el fin de analizar la evolución temporal de alfa y beta, calcule estos parámetros
# para cada año y para cada stock. Luego, grafique su evolución en el tiempo e interprete los resultados.

df_dinamico <- data.frame(Fecha = index(series_excesos), coredata(series_excesos))
df_dinamico$Year <- year(df_dinamico$Fecha) # Se extrae el año para agrupar los cálculos anualmente.

df_largo <- df_dinamico %>%
  pivot_longer(cols = c("AAPL_ex", "MSFT_ex", "AMZN_ex"),
               names_to = "Activo",
               values_to = "Exceso_Activo") # Formato largo es necesario para separar paneles en ggplot2.

# Se corre una regresión distinta para cada año y cada empresa.
resultados_anuales <- df_largo %>%
  group_by(Year, Activo) %>%
  summarise(
    Alpha = coef(lm(Exceso_Activo ~ Mercado_ex))[1], # Intercepto anual.
    Beta  = coef(lm(Exceso_Activo ~ Mercado_ex))[2], # Pendiente anual.
    .groups = "drop" # Desagrupa para evitar errores posteriores.
  )

graphics.off()

# Gráfico de evolución del alpha
grafico_alpha <- ggplot(resultados_anuales, aes(x = Year, y = Alpha, color = Activo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") + # Línea de referencia CAPM (Alpha teórico = 0).
  facet_wrap(~Activo, ncol = 1) + # Separa el lienzo en 3 gráficos verticales (uno por acción).
  theme_minimal() +
  labs(title = "Alpha evolution over time", x = "Year", y = "Estimated alpha") +
  theme(legend.position = "none")

ggsave("evolucion_alpha.jpg", plot = grafico_alpha, width = 8, height = 6)

# Gráfico de evolución del beta
grafico_beta <- ggplot(resultados_anuales, aes(x = Year, y = Beta, color = Activo)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "black") + # Línea de referencia (beta = 1 implica mismo riesgo que el mercado).
  facet_wrap(~Activo, ncol = 1) +
  theme_minimal() +
  labs(title = "Beta evolution over time", x = "Year", y = "Estimated beta") +
  theme(legend.position = "none")

ggsave("evolucion_beta.jpg", plot = grafico_beta, width = 8, height = 6)

# PARTE D: Testee la hipótesis nula beta = 0 para cada empresa (los encontrados en el ítem b).
# Interprete sus resultados y relaciónelos con la teoría financiera subyacente al modelo CAPM.

# Se encapsula el análisis estadístico en una función para no repetir código.
test_hipotesis_beta <- function(modelo, nombre_activo) {

  coeficientes <- summary(modelo)$coefficients # Extrae la matriz de resultados t, errores, etc.
  estimacion_beta <- coeficientes[2, "Estimate"] # Valor puntual del beta.
  estadistico_t <- coeficientes[2, "t value"] # Cuántas desviaciones se aleja de cero.
  p_valor <- coeficientes[2, "Pr(>|t|)"] # Probabilidad de error al rechazar la hipótesis nula.

  cat(sprintf("\n Hypothesis test for %s \n", nombre_activo))
  cat(sprintf("H0: Beta = 0  vs  H1: Beta != 0\n"))
  cat(sprintf("Estimated beta: %.4f\n", estimacion_beta))
  cat(sprintf("t-statistic:    %.4f\n", estadistico_t))
  cat(sprintf("P-value:        %e\n", p_valor))

  # Lógica de decisión al 5% de nivel de significancia estadística.
  if(p_valor < 0.05) {
    cat("Conclusion: REJECT the null hypothesis (H0) at the 5% significance level.\n")
    cat("Interpretation: The asset has a statistically significant relationship with the market.\n")
  } else {
    cat("Conclusion: DO NOT REJECT the null hypothesis (H0) at the 5% significance level.\n")
    cat("Interpretation: There is no sufficient evidence to conclude that the asset moves with the market.\n")
  }
}

# Ejecuta el test para las 3 acciones.
test_hipotesis_beta(modelo_aapl, "Apple (AAPL)")
test_hipotesis_beta(modelo_msft, "Microsoft (MSFT)")
test_hipotesis_beta(modelo_amzn, "Amazon (AMZN)")
