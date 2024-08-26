# Instalar y cargar los paquetes necesarios
packages <- c("haven", "forecast", "ggplot2", "dplyr", "magrittr", "TTR", "tseries", "urca")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(packages, library, character.only = TRUE)

# Instalar el paquete (si no está instalado)
install.packages("readr")

# Cargar el paquete
library(readr)

# Leer el archivo CSV como serie
serie.precio <- read.csv("C:/Users/56988/Desktop/ClasesDS/Series_tiempo_Rstudio/sesion05/arreglovariables/ipc.csv")

# Revisar datos
head(serie.precio)
tail(serie.precio)

# Generar la Serie Temporal
serie.precio.ts <- ts(serie.precio$PN38712PM, start = c(1992, 1), frequency = 12)

# Graficar la serie temporal
autoplot(serie.precio.ts) +
  ggtitle("Evolución del IPC en Lima Y, enero 1992 - sept 2023") +
  xlab("Años/Meses") +
  ylab("IPC")

# Realizar pruebas de raíz unitaria
# Prueba de Dickey-Fuller Aumentada (ADF)
adf_test <- adf.test(serie.precio.ts, alternative="stationary")
print(adf_test)

# H0: La serie tiene una raíz unitaria (no es estacionaria); 
# H1: La serie no tiene una raíz unitaria (es estacionaria); 
# si -3.6673 es menor que -2,8699, H0 se rechaza; si -3.6673 es mayor 
# que -2,8699, H0 no se rechaza; por lo tanto, la serie no tiene una 
# raíz unitaria, lo cual es estacionaria.

# Estadístico de prueba: -3.6673
# Valor p (probabilidad): 0.02669 *el p-valor te dice cuán probable 
# es observar los datos que tienes si la serie realmente tiene 
# una raíz unitaria (es decir, no es estacionaria)
# nivel de significancia: 0,05 (-2.8699298018856574)
# Lag order = 7, significa las 7 veces de resagos de la serie.

# Prueba de Phillips-Perron (PP)
# Nota: La función adf.test realiza la prueba ADF y PP en la versión moderna de tseries
pp_test <- adf.test(serie.precio.ts, alternative="stationary", k=0)  # k=0 para PP
print(pp_test)

# H0: La serie tiene una raíz unitaria (no es estacionaria); 
# H1: La serie no tiene una raíz unitaria (es estacionaria); 
# si -4.2171 es menor que -3,45, H0 se rechaza; si -4.2171 es mayor 
# que -3,45, H0 no se rechaza; por lo tanto, la serie no tiene una 
# raíz unitaria, lo cual es estacionaria.
# Para 5% de significancia: -2,87
# Para 1% de significancia: -3,45

# Prueba de Kwiatkowski-Phillips-Schmidt-Shin (KPSS)
kpss_test <- ur.kpss(serie.precio.ts)
summary(kpss_test)


# Critical value for a significance level of: 
#  10pct  5pct 2.5pct  1pct
# critical values 0.347 0.463  0.574 0.739

# Hipótesis nula (H0): La serie es estacionaria alrededor 
# de una tendencia o media.
# Hipótesis alternativa (H1): La serie tiene una raíz unitaria 
# (es no estacionaria).
# Si 6.0145 es menor que los valores críticos al 5%, 
# H0 no se rechaza; si 6.0145 es mayor 
# que valores críticos, H0 se rechaza; por lo tanto, 
# la serie es no estacionaria.


# Test de Zivot-Andrews
za_test <- ur.za(serie.precio.ts, model = "both", lag = 0)
summary(za_test)

# Según esta prueba, la serie de tiempo es estacionaria con una ruptura
# en la posición 353 (por eso la contradicción de las 3 pruebas anteriores).

# Ajustar el modelo ARIMA si la serie es estacionaria
# En caso contrario, realizar diferenciación
if (adf_test$p.value > 0.05) {
  serie.precio.ts_diff <- diff(serie.precio.ts)
  adf_test_diff <- adf.test(serie.precio.ts_diff, alternative="stationary")
  print(adf_test_diff)
  
  # Ajustar el modelo ARIMA en la serie diferenciada
  arima_model <- auto.arima(serie.precio.ts_diff)
} else {
  # Ajustar el modelo ARIMA en la serie original si ya es estacionaria
  arima_model <- auto.arima(serie.precio.ts)
}

# Resumen del modelo ARIMA
summary(arima_model)

# Correlogramas simple y parcial
acf(serie.precio.ts)
pacf(serie.precio.ts)

# Aquí no se realizó la diferenciación, porque no se cumplió en el 
# modelo de la serie diferenciada

# Correlogramas simple y parcial de la serie diferenciada
acf(serie.precio.ts_diff)
pacf(serie.precio.ts_diff)


# Diagnóstico del modelo
checkresiduals(arima_model)

# Predicciones
forecast_arima <- forecast(arima_model, h = 12)  # Predecir los próximos 12 períodos

# Graficar las predicciones
autoplot(forecast_arima) +
  ggtitle("Predicciones ARIMA del IPC Y") +
  xlab("Años/Meses") +
  ylab("IPC")


# Intento de ajuste nuevo, separando el modelo ARIMA
# Ajustar diferentes modelos ARIMA
arima_model1 <- auto.arima(serie.precio.ts, seasonal=FALSE)
arima_model2 <- auto.arima(serie.precio.ts, seasonal=TRUE)

# Comparar AIC y BIC
AIC(arima_model1, arima_model2)
BIC(arima_model1, arima_model2)

# Diagnóstico de residuos
checkresiduals(arima_model1)
checkresiduals(arima_model2)

# Seleccionar el mejor modelo basado en AIC, BIC y diagnósticos de residuos
best_model <- ifelse(AIC(arima_model1) < AIC(arima_model2), arima_model1, arima_model2)

# Predicciones con el mejor modelo
forecast_arima <- forecast(best_model, h=12)
autoplot(forecast_arima) +
  ggtitle("Predicciones ARIMA del IPC Y") +
  xlab("Años/Meses") +
  ylab("IPC")
