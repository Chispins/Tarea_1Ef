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

# Tarea 2 - Econometría financiera (Pregunta 1)

library(lmtest)   # resettest()
library(AER)      # waldtest()

# Preparar df para Tarea 2 (viene de series_excesos creado en Tarea 1)
df <- as.data.frame(series_excesos)
df$Mercado_ex2 <- df$Mercado_ex^2

# PARTE A: Existen extensiones del modelo CAPM que introducen términos cuadráticos para capturar efectos no lineales
# en la relación entre el retorno de un activo y el retorno del mercado. Esto puede
# ser útil en mercados con alta volatilidad y cuando algunos activos reaccionan de manera diferente en
# el mercado dependiendo de su estado. Se puede modelar de la forma (Modelo 2): (Ri − Rf) = γ+δ1(Rm − Rf)+δ2(Rm − Rf)2 +µ
# Obtenga los coeficientes γ, δ1 y δ2 e interprete estos coeficientes

cat("\n\n")
cat("PART A: Estimation of Model 2 (Quadratic CAPM)\n")
cat("\n\n")

# AAPL
cat("\n Model 2: AAPL \n")
modelo2_aapl <- lm(AAPL_ex ~ Mercado_ex + Mercado_ex2, data = df)
print(summary(modelo2_aapl))

# MSFT
cat("\n Model 2: MSFT \n")
modelo2_msft <- lm(MSFT_ex ~ Mercado_ex + Mercado_ex2, data = df)
print(summary(modelo2_msft))

# AMZN
cat("\n Model 2: AMZN \n")
modelo2_amzn <- lm(AMZN_ex ~ Mercado_ex + Mercado_ex2, data = df)
print(summary(modelo2_amzn))

# Tabla resumen Modelo 2
tabla_m2 <- data.frame(
  Asset  = c("AAPL", "MSFT", "AMZN"),
  Gamma  = c(coef(modelo2_aapl)[1], coef(modelo2_msft)[1], coef(modelo2_amzn)[1]),
  Delta1 = c(coef(modelo2_aapl)[2], coef(modelo2_msft)[2], coef(modelo2_amzn)[2]),
  Delta2 = c(coef(modelo2_aapl)[3], coef(modelo2_msft)[3], coef(modelo2_amzn)[3])
)
cat("\n Summary of coefficients Model 2 \n")
print(tabla_m2)

# PARTE B: Compare los coeficientes anteriores con los coeficientes α y β del Modelo 1 obtenidos en
# la tarea anterior (Práctica 1, Pregunta 1). Considerando todos los coeficientes obtenidos y tomando
# un punto de vista económico-financiero, ¿cuál modelo le parece se acerca mejor a la realidad de los
# datos? Justifique.

cat("\n\n")
cat("PART B: Comparison M1 vs M2\n")
cat("\n\n")

# Modelo 1 estimado en df (mismo marco de datos para una comparación clara)
modelo1_aapl <- lm(AAPL_ex ~ Mercado_ex, data = df)
modelo1_msft <- lm(MSFT_ex ~ Mercado_ex, data = df)
modelo1_amzn <- lm(AMZN_ex ~ Mercado_ex, data = df)

tabla_comparacion <- data.frame(
  Asset         = rep(c("AAPL", "MSFT", "AMZN"), each = 2),
  Model         = rep(c("M1 (linear)", "M2 (quadratic)"), times = 3),
  Alpha_or_Gamma = c(
    coef(modelo1_aapl)[1], coef(modelo2_aapl)[1],
    coef(modelo1_msft)[1], coef(modelo2_msft)[1],
    coef(modelo1_amzn)[1], coef(modelo2_amzn)[1]
  ),
  Beta_or_Delta1 = c(
    coef(modelo1_aapl)[2], coef(modelo2_aapl)[2],
    coef(modelo1_msft)[2], coef(modelo2_msft)[2],
    coef(modelo1_amzn)[2], coef(modelo2_amzn)[2]
  ),
  Delta2 = c(
    NA, coef(modelo2_aapl)[3],
    NA, coef(modelo2_msft)[3],
    NA, coef(modelo2_amzn)[3]
  ),
  Adj_R2 = c(
    summary(modelo1_aapl)$adj.r.squared, summary(modelo2_aapl)$adj.r.squared,
    summary(modelo1_msft)$adj.r.squared, summary(modelo2_msft)$adj.r.squared,
    summary(modelo1_amzn)$adj.r.squared, summary(modelo2_amzn)$adj.r.squared
  )
)

cat("\n Comparison table M1 vs M2:\n")
print(tabla_comparacion)

cat("\n Delta2 significance by asset \n")
for (nm in list(list("AAPL", modelo2_aapl),
                list("MSFT", modelo2_msft),
                list("AMZN", modelo2_amzn))) {
  s <- summary(nm[[2]])$coefficients
  cat(sprintf("%s: delta2 = %.5f,  t = %.4f,  p-value = %.4f\n",
              nm[[1]], s[3, "Estimate"], s[3, "t value"], s[3, "Pr(>|t|)"]))
}

# PARTE C: Utilice los criterios de selección de modelos discutidos en clases
# y muestre cuál es el modelo elegido (Modelo 1 o Modelo 2) con cada uno de esos criterios.
# Explique y fundamente su respuesta

cat("\n\n")
cat("PART C: Model Selection Criteria\n")
cat("\n\n")

# Hannan-Quinn (no incluido en la base R AIC / BIC, calculado manualmente)
HQ <- function(modelo) {
  n  <- length(residuals(modelo))
  k  <- length(coef(modelo))
  s2 <- sum(residuals(modelo)^2) / n
  log(s2) + 2 * k * log(log(n)) / n
}

compute_criteria <- function(m1, m2, name) {
  cat(sprintf("\n %s \n", name))
  cat(sprintf("%-20s %12s %12s\n", "Criterion", "Model 1", "Model 2"))
  cat(sprintf("%-20s %12.6f %12.6f\n", "Adjusted R2",
              summary(m1)$adj.r.squared, summary(m2)$adj.r.squared))
  cat(sprintf("%-20s %12.4f %12.4f\n", "AIC", AIC(m1), AIC(m2)))
  cat(sprintf("%-20s %12.4f %12.4f\n", "BIC", BIC(m1), BIC(m2)))
  cat(sprintf("%-20s %12.4f %12.4f\n", "HQ",  HQ(m1),  HQ(m2)))

  cat(sprintf("  -> Adj. R2 prefers: %s\n",
              ifelse(summary(m2)$adj.r.squared > summary(m1)$adj.r.squared, "M2", "M1")))
  cat(sprintf("  -> AIC     prefers: %s\n", ifelse(AIC(m2) < AIC(m1), "M2", "M1")))
  cat(sprintf("  -> BIC     prefers: %s\n", ifelse(BIC(m2) < BIC(m1), "M2", "M1")))
  cat(sprintf("  -> HQ      prefers: %s\n", ifelse(HQ(m2)  < HQ(m1),  "M2", "M1")))
}

compute_criteria(modelo1_aapl, modelo2_aapl, "AAPL")
compute_criteria(modelo1_msft, modelo2_msft, "MSFT")
compute_criteria(modelo1_amzn, modelo2_amzn, "AMZN")

# PARTE D: Suponga que volvemos al modelo original (Modelo 1):
# (Ri − Rf) = α+β(Rm − Rf)+ϵ
# Aplique un test de Ramsey-RESET para detectar si habían originalmente no linealidades omitidas
# en este Modelo 1. Dado el resultado de este test, ¿el Modelo 2 es razonable? Discuta.

cat("\n\n")
cat("PART D: Ramsey RESET Test\n")
cat("\n\n")

apply_reset <- function(modelo, name) {
  cat(sprintf("\n RESET Test: %s \n", name))
  r2  <- resettest(modelo, power = 2,   type = "fitted")
  r3  <- resettest(modelo, power = 3,   type = "fitted")
  r23 <- resettest(modelo, power = 2:3, type = "fitted")
  cat(sprintf("  RESET (yhat^2 only):     F = %7.4f,  p-value = %.4f\n",
              r2$statistic,  r2$p.value))
  cat(sprintf("  RESET (yhat^3 only):     F = %7.4f,  p-value = %.4f\n",
              r3$statistic,  r3$p.value))
  cat(sprintf("  RESET (yhat^2 + yhat^3): F = %7.4f,  p-value = %.4f\n",
              r23$statistic, r23$p.value))
  cat(sprintf("  Conclusion: %s\n",
              ifelse(r23$p.value < 0.05,
                     "REJECT H0: omitted nonlinearities in Model 1 (5%)",
                     "FAIL TO REJECT H0: no evidence of nonlinearities (5%)")))
}

apply_reset(modelo1_aapl, "AAPL")
apply_reset(modelo1_msft, "MSFT")
apply_reset(modelo1_amzn, "AMZN")

# Gráficos: Dispersión M1 vs M2
graphics.off()

plot_fit <- function(data, asset, m1, m2, color1, color2) {
  y_col   <- paste0(asset, "_ex")
  df_plot <- data.frame(
    x  = data$Mercado_ex,
    y  = data[[y_col]],
    m1 = fitted(m1),
    m2 = fitted(m2)
  ) %>% arrange(x)

  ggplot(df_plot, aes(x = x, y = y)) +
    geom_point(alpha = 0.3, color = "gray50", size = 1) +
    geom_line(aes(y = m1), color = color1, linewidth = 1, linetype = "solid") +
    geom_line(aes(y = m2), color = color2, linewidth = 1, linetype = "dashed") +
    labs(
      title   = sprintf("Fit M1 vs M2: %s", asset),
      x       = "Market excess return",
      y       = sprintf("%s excess return", asset),
      caption = "Solid = M1 (linear)  |  Dashed = M2 (quadratic)"
    ) + theme_minimal()
}

ggsave("ajuste_AAPL.jpg",
       plot = plot_fit(df, "AAPL", modelo1_aapl, modelo2_aapl, "blue",      "darkblue"),
       width = 8, height = 5)
ggsave("ajuste_MSFT.jpg",
       plot = plot_fit(df, "MSFT", modelo1_msft, modelo2_msft, "darkgreen", "olivedrab"),
       width = 8, height = 5)
ggsave("ajuste_AMZN.jpg",
       plot = plot_fit(df, "AMZN", modelo1_amzn, modelo2_amzn, "purple",    "darkorchid"),
       width = 8, height = 5)
