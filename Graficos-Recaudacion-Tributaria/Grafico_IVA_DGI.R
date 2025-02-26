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
library(x13binary)
library(seasonal)
library(dygraphs)
library(xts)
library(htmlwidgets)

setwd("C:/Users/DELL/OneDrive/Desktop/MECON/Graficos")
IVA <- read_excel("Serie_RECA.xlsx")
IVA <- IVA %>% 
  select(Anio, IVA_DGI)
colnames(IVA)[1] <- "Fecha"

#deflactar la serie

IPC <- read_excel("serie larga IPC.xlsx")
IPC <- IPC %>% select(-c(1,4,5,6))
colnames(IPC)[1] <- "Fecha"
colnames(IPC)[2] <- "IPC"
Serie_IVA_DGI <- inner_join(IVA, IPC, by = "Fecha")
Serie_IVA_DGI <- Serie_IVA_DGI %>%
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))

#extraer el ultimo dato del ipc
ipc_actual <- tail(Serie_IVA_DGI$IPC, 1)

#ajuste por ipc
Serie_IVA_DGI <- Serie_IVA_DGI %>%
  mutate(IVA_DGI_deflactado = IVA_DGI * ipc_actual / IPC)
IVA <- Serie_IVA_DGI[,-2:-3] #Serie_IVA_DGI contiene la de $ corrientes y constantes

# Crear la serie temporal solo para la serie real
ts_IVA <- ts(IVA$IVA_DGI_deflactado, start = c(1997, 1), frequency = 12)

# Descomponer y desestacionalizar
decomp <- decompose(ts_IVA, type = "multiplicative")
IVA_DGI_SIN_EST <- ts_IVA / decomp$seasonal  # Serie desestacionalizada

# Crear columna de fecha
fechas <- seq(as.Date("1997/1/1"), by = "month", length.out = nrow(IVA))

# Crear el dataframe final con tres columnas: Fecha, Serie Original, Serie Desestacionalizada
IVA_desest <- IVA %>%
  mutate(Fecha = fechas, IVA_DGI_SIN_EST = as.numeric(IVA_DGI_SIN_EST))

# Reorganizar las columnas
IVA_desest <- IVA_desest %>%
  select(Fecha, IVA_DGI_deflactado, IVA_DGI_SIN_EST)

######variacion real mensual para otro grafico

IVA_variaciones <- IVA_desest %>%
  mutate(
    Variacion_IVA_DGI = (IVA_DGI_deflactado / lag(IVA_DGI_deflactado) - 1) * 100,  # C?lculo de variaci?n porcentual en %
    Variacion_IVA_DGI_SIN_EST = (IVA_DGI_SIN_EST / lag(IVA_DGI_SIN_EST) - 1) * 100  
  ) %>%
  select(Fecha, Variacion_IVA_DGI, Variacion_IVA_DGI_SIN_EST)  # Seleccionar solo las columnas deseadas


