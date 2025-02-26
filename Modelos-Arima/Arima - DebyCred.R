library(readxl)
library(tidyverse)
library(ggthemes)
library(foreign)
library(svglite)
library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(readr)
library(date)
library(writexl)
library(zoo)
library(scales)
library(data.table)
library(httr)
library(openxlsx)
library(foreign)

# install.packages("urca")
# install.packages("forecast")
# install.packages("strucchange")

library(urca)
library(forecast)
library(strucchange)
library(tseries)

#INTERNOS

#Poner 1 si no se usa la recaudación efectiva. 0 si se usa la efectiva.
Rec <- 0


DebyCred <- read.xlsx("X:/Y/ComExt/Lis y Nico/CredyDeb/Serie_Cred_y_Deb.xlsx", colNames = FALSE)

DebyCred  <- DebyCred [,-2] #borro serie nominal uso la real

DebyCred  <- DebyCred [-1,]

colnames(DebyCred)[1] <- "Mes"
colnames(DebyCred)[2] <- "DebyCred"

Fecha <- seq(as.Date(paste0(year(Sys.time()),"/", month(Sys.time() %m+% months(Rec)),"/01")), by = "month", length.out = 24)

Estimaciones_DebyCred <- data.frame(Fecha)

DebyCred$Fecha <- seq(from = as.Date("2004/01/01"), by = "month", length.out = nrow(DebyCred))
DebyCred <- DebyCred[,-1]
DebyCred <- DebyCred[, c(2, 1)]

serie_emae <- read.xlsx("X:/Y/ComExt/Lis y Nico/CredyDeb/serie_EMAE.xlsx")
serie_emae$Fecha <- seq(from = as.Date("2004/01/01"), by = "month", length.out = nrow(serie_emae))
serie_emae <- serie_emae[,-1]
serie_emae <- serie_emae[, c(2, 1)]

serie_dias_habiles <- read.xlsx("X:/Y/ComExt/Lis y Nico/CredyDeb/serie_dias_habiles.xlsx")
serie_dias_habiles$Fecha <- seq(from = as.Date("2004/01/01"), by = "month", length.out = nrow(serie_dias_habiles))
serie_dias_habiles <- serie_dias_habiles[,-1]
serie_dias_habiles <- serie_dias_habiles[, c(2, 1)]

regresores <- cbind(serie_dias_habiles, serie_emae)
regresores <- regresores[,-3]
regresores <- regresores[,-1]
colnames(regresores)[2] <- "EMAE"
colnames(regresores)[1] <- "Dias Habiles"
regresores <- as.matrix(regresores)

#######
# estimaciones de regresores

forecast_emae <- read.xlsx("X:/Y/ComExt/Lis y Nico/CredyDeb/serie_EMAE_proyeccion.xlsx")
forecast_emae <- forecast_emae[,-3:-4]
forecast_dias_habiles <- read.xlsx("X:/Y/ComExt/Lis y Nico/CredyDeb/serie_dias_habiles_proyeccion.xlsx")
colnames(forecast_emae)[2] <- "EMAE"
colnames(forecast_dias_habiles)[2] <- "Dias Habiles"
forecast_emae <- forecast_emae[,-1]
forecast_dias_habiles <- forecast_dias_habiles[,-1]

forecast_regresores <- cbind(forecast_dias_habiles, forecast_emae)

colnames(forecast_regresores)[1] <- "Dias Habiles"
colnames(forecast_regresores)[2] <- "EMAE"
forecast_regresores <- as.data.frame(forecast_regresores)
forecast_regresores <- as.matrix(forecast_regresores)
view(forecast_regresores)


ts_DebyCred <- ts(log(as.numeric(DebyCred$DebyCred)), start = c(2004, 1), frequency = 12)

Modelo_DebyCred <- auto.arima(ts_DebyCred)
summary(Modelo_DebyCred)
Modelo_DebyCred2 <- Arima(ts_DebyCred, order = c(2,1,2), seasonal = c(0,0,2), method = "ML", xreg = regresores)

# Prueba de estacionariedad con ADF (Dickey-Fuller aumentado)
adf.test(residuals(Modelo_DebyCred2))

# Test de normalidad de los residuos (Jarque-Bera)
jarque.bera.test(residuals(Modelo_DebyCred2))

# Test de autocorrelación de residuos (Ljung-Box)
Box.test(residuals(Modelo_DebyCred2), type = "Ljung-Box", lag = 10)

# Extraer los residuos del modelo
residuos <- residuals(Modelo_DebyCred2)

# Graficar la función de autocorrelación (ACF)
acf(residuos, main = "Autocorrelación de los residuos (ACF)")

# Graficar la función de autocorrelación parcial (PACF)
pacf(residuos, main = "Autocorrelación parcial de los residuos (PACF)")

#Proyeccion

Forecast_DebyCred2 <- forecast(Modelo_DebyCred2, xreg = forecast_regresores, h = nrow(forecast_regresores))
Estimaciones_DebyCred2 <- exp(Forecast_DebyCred2$mean)

view(Estimaciones_DebyCred2)
Estimaciones_DebyCred2 <- as.numeric(unlist(Estimaciones_DebyCred2))
Estimaciones_DebyCred2 <- as.data.frame(Estimaciones_DebyCred2)
colnames(Estimaciones_DebyCred2)[1] <- "DebyCred"

view(Estimaciones_DebyCred2)
plot(Forecast_DebyCred2)

Estimaciones_DebyCred2$Fecha <- seq(from = as.Date("2025/02/01"), by = "month", length.out = nrow(Estimaciones_DebyCred2))
Estimaciones_DebyCred2 <- Estimaciones_DebyCred2[,c(2,1)]
write.xlsx(Estimaciones_DebyCred2, "Esti_DebyCred_feb25.xlsx")



