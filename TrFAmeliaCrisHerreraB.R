# Trabajo final, Regresión lineal con series de tiempo.
# Amelia Cristina Herrera Briceño, melinnicri@gmail.com
# Precios IPC.

# Análisis de descomposición:

# Instalar y cargar los paquetes necesarios
packages <- c("haven", "forecast", "ggplot2", "dplyr", "magrittr")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if (length(new_packages)) install.packages(new_packages)

lapply(packages, library, character.only = TRUE)


# Cargar el paquete
library(readr)

# Leer el archivo CSV como serie
serie.precio <- read.csv("C:/Users/56988/Desktop/ClasesDS/Series_tiempo_Rstudio/sesion05/arreglovariables/ipc.csv", sep = ",", dec = ".", encoding = "Latin1")

# fecha y PN38712PM
serie.precio

# Verificar la estructura de los datos
str(serie.precio)

# Mostrar las primeras filas de los datos
head(serie.precio)

# Mostrar las últimas filas
tail(serie.precio)

# Crear la serie temporal (reemplaza con tus datos si es necesario)
serie.precio.ts <- ts(serie.precio$PN38712PM, start = c(1992, 1), frequency = 12)

# Verificar la serie temporal
print(serie.precio.ts)

# Graficar la serie temporal
plot(serie.precio.ts)

install.packages("forecast")
install.packages("ggplot2")

library(forecast)
library(ggplot2)


# Graficamos la serie temporal
autoplot(serie.precio.ts) +
  ggtitle("Evolución del IPC Y, Ene 1992 - Sep 2023") +
  xlab("Años/Meses") +
  ylab("Índice IPC")

####################
## Modelo Aditivo ##
####################

# 1.1. Componente de Tendencia Tt [Componente TCt]
ma2_12 <- ma(serie.precio.ts, order = 12, centre = TRUE)
autoplot(serie.precio.ts) +
  ggtitle("Evolución del IPC Y, Ene 1992 - Sep 2023") +
  xlab("Años/Meses") +
  ylab("Índice IPC") +
  autolayer(ma2_12, series = "Tendencia (Tt)")

# 1.2. Cálculos de los Factores Estacionales [Componente St]
# Calcular la serie ajustada (SI) eliminando la tendencia
Yt <- serie.precio.ts  # serie original
Tt <- ma2_12           # media móvil centrada
SI <- Yt - Tt         # Diferencia: componente estacional e irregular

# Calcular el componente estacional (St) como el promedio de los residuos
St <- tapply(SI, cycle(SI), mean, na.rm = TRUE)

# Ajustar los factores estacionales para que sumen 0
St <- St - mean(St)

# Mostrar los factores estacionales en consola
print("Factores Estacionales:")
print(St)

# Generar la serie de factores estacionales ajustados para la serie temporal completa
St_series_adjusted <- rep(St, length.out = length(Yt)) %>% ts(start = start(serie.precio.ts), frequency = 12)

# Graficar los factores estacionales ajustados
autoplot(St_series_adjusted) +
  ggtitle("Factores Estacionales Ajustados") +
  xlab("Años/Meses") +
  ylab("Factor Estacional")


# 1.3. Cálculo del Componente Irregular [It]; It = Yt - Tt - St
It <- Yt - Tt - St_series_adjusted

# Graficar el componente irregular
autoplot(It) +
  ggtitle("Componente Irregular (It)") +
  xlab("Años/Meses") +
  ylab("Componente Irregular")


# 1.4. Descomposicion Aditiva (usando la libreria Stats)

descomposicion_aditiva<-decompose(serie.precio.ts,type = "additive")
autoplot(descomposicion_aditiva,main="Descomposicion Aditiva",
        xlab="Años/Meses")

# 1.5. Descomposicion Aditiva (usando libreria feasts)

library(tsibble)
library(feasts)
library(ggplot2)
Yt%>%as_tsibble()%>% #convertir serie temporal a tsibble
  model(             #convertir a modelo de descomposicion clasica
    classical_decomposition(value,type = "additive")
  )%>%
  components()%>%   #pedimos que extraiga los componentes
  autoplot() +      #pedimos que grafique
  labs(title = "Descomposicion Clasica Aditiva, precio")+
  xlab("Años/Meses")


###########################
## Modelo Multiplicativo ##
##########################

# 2.1. Componente Tendencia ciclo [Tt=TCt]

Tt<- ma(serie.precio.ts, 12, centre = TRUE)
autoplot(Tt,main = "Componente Tendencia [Ciclo]", xlab = "Años/Meses",ylab = "Tt")

# 2.2. Calculos de Factores Estacionales [St]

SI<-Yt/Tt                  #Serie sin tendencia.

# Calcular los factores estacionales (St) promediando los resultados de cada mes
St <- tapply(SI, cycle(SI), mean, na.rm = TRUE)

# Ajustar los factores estacionales para que sumen "1" en el modelo multiplicativo
St <- St * 12 / sum(St)

# Generar la serie de factores estacionales para cada valor de la serie original
St_series <- rep(St, length.out = length(Yt)) %>% ts(start = start(serie.precio.ts), frequency = 12)

# Mostrar los factores estacionales en consola
print("Factores Estacionales:")
print(St)

# Graficar los factores estacionales
autoplot(St_series) +
  ggtitle("Factores Estacionales") +
  xlab("Años/Meses") +
  ylab("Factor Estacional")


# 2.3. Calculo del componente Irregular [It]

It <- Yt / (Tt * St_series)

# Graficar el componente irregular
autoplot(It) +
  ggtitle("Componente Irregular (It)") +
  xlab("Años/Meses") +
  ylab("Componente Irregular")


# 2.4. Descomposicion multiplicativa (usando libreria stats)

descomposicion_multiplicatica<-decompose(serie.precio.ts,type = "multiplicative")
autoplot(descomposicion_multiplicatica,main="Descomposición Multiplicativa",xlab="Años/Meses")


# 2.5. Descomposicion multiplicativa (usando libreria feasts)

library(tsibble)
library(feasts)
library(ggplot2)
Yt %>% as_tsibble() %>%
  model(classical_decomposition(value, type = "multiplicative")) %>%
  components() %>%
  autoplot() +
  labs(title = "Descomposición Clásica Multiplicativa, precio") + xlab("Años/Meses")


######### 
# Descomposicion (usando la libreria TSstudio)
######### 

# Intenta instalar TSstudio desde CRAN
install.packages("TSstudio", repos = "https://cloud.r-project.org/")

# Verifica que el paquete esté instalado
if ("TSstudio" %in% rownames(installed.packages())) {
  # Cargar el paquete
  library(TSstudio)
} else {
  print("El paquete TSstudio no se pudo instalar. Verifica los errores de instalación.")
}

library(TSstudio)
# Aditivo
ts_decompose(Yt, type = "additive", showline = TRUE)
# Multiplicativo
ts_decompose(Yt, type = "multiplicative", showline = TRUE)







