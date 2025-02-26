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

setwd("C:/Users/DELL/OneDrive/Desktop/MECON/Graficos")
Cred_y_Deb <- read_excel("Serie_RECA.xlsx")
Cred_y_Deb <- Cred_y_Deb %>% 
  select(Anio, Creditos_y_Debitos_en_Cta._Cte.)

#borramos filas con ceros para años donde no regia el impuesto
Cred_y_Deb <- Cred_y_Deb %>%
  group_by(Anio) %>% # Agrupar por año
  filter(!(Anio <= min(Anio[Creditos_y_Debitos_en_Cta._Cte. != 0]) & Creditos_y_Debitos_en_Cta._Cte. == 0)) %>% #Eliminar filas de años con todos ceros antes del primer no-cero
  ungroup()
colnames(Cred_y_Deb)[1] <- "Fecha"
#deflactar la serie

IPC <- read_excel("serie larga IPC.xlsx")
IPC <- IPC %>% select(-c(1,4,5,6))
colnames(IPC)[1] <- "Fecha"
colnames(IPC)[2] <- "IPC"
Serie_Cred_y_Deb <- inner_join(Cred_y_Deb, IPC, by = "Fecha")
Serie__Cred_y_Deb <- Serie_Cred_y_Deb %>%
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))
colnames(Serie_Cred_y_Deb)[2] <- "Cred_y_Deb"

##agregar dato de inflacion ultimo mes a mano
# infla_septiembre <- tail(data$IPC, 1)*1.04
#IPC <- rbind(IPC, infla_septiembre, 2)

# Extraer el valor del IPC del ?ltimo mes
ipc_actual <- tail(Serie_Cred_y_Deb$IPC, 1)

#ajuste por ipc
Serie_Cred_y_Deb <- Serie_Cred_y_Deb %>%
  mutate(Cred_y_Deb_deflactado = Cred_y_Deb * ipc_actual / IPC)
Cred_y_Deb <- Serie_Cred_y_Deb[,-2:-3] #Serie_Cred_y_Deb contiene la de $ corrientes y constantes

# Crear la serie temporal
ts_Cred_y_Deb <- ts(Cred_y_Deb$Cred_y_Deb_deflactado, start = c(2001, 4), frequency = 12)

# Descomponer y desestacionalizar
decomp <- decompose(ts_Cred_y_Deb, type = "multiplicative")
Cred_y_Deb_SIN_EST <- ts_Cred_y_Deb / decomp$seasonal  # Serie desestacionalizada

# Crear columna de fecha
fechas <- seq(as.Date("2001/4/1"), by = "month", length.out = nrow(Cred_y_Deb))

# Crear el dataframe final con tres columnas: Fecha, Serie Original, Serie Desestacionalizada
Cred_y_Deb_SIN_EST <- Cred_y_Deb %>%
  mutate(Fecha = fechas, Cred_y_Deb_SIN_EST = as.numeric(Cred_y_Deb_SIN_EST))

# Reorganizar las columnas
Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
  select(Fecha, Cred_y_Deb_deflactado, Cred_y_Deb_SIN_EST)


# Calcular la variaci?n porcentual mensual y convertir a porcentaje
Cred_y_Deb_variaciones <- Cred_y_Deb_SIN_EST %>%
  mutate(
    Variacion_Cred_y_Deb = (Cred_y_Deb_deflactado / lag(Cred_y_Deb_deflactado) - 1) * 100,  # C?lculo de variaci?n porcentual en %
    Variacion_Cred_y_Deb_SIN_EST = (Cred_y_Deb_SIN_EST / lag(Cred_y_Deb_SIN_EST) - 1) * 100  # C?lculo de variaci?n porcentual en %
  ) %>%
  select(Fecha, Variacion_Cred_y_Deb, Variacion_Cred_y_Deb_SIN_EST)  # Seleccionar solo las columnas deseadas

# # Calcular variaci?n interanual
# Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
#   mutate(Var_Interanual_deses = (Cred_y_Deb_deses / lag(Cred_y_Deb_deses, 12) - 1) * 100)
# 
# # Vamos a agregar la suma acumulada trimestral y la variaci?n interanual
# 
# Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
#   arrange(Fecha) %>%
#   mutate(Suma_Trimestral = rollapply(Cred_y_Deb_deses, width = 3, FUN = sum, align = "right", fill = NA)) %>%
#   mutate(Var_Interanual_Trimestral = (Suma_Trimestral / lag(Suma_Trimestral, 12) - 1) * 100)
# 
# # Limpiar y renombrar columnas
# Serie_Cred_y_Deb <- data_real %>%
#   select(Fecha, Cred_y_Deb = Cred_y_Deb_real, Var_Interanual)

