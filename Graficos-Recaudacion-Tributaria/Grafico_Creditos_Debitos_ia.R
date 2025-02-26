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
Serie_Cred_y_Deb <- read_excel("Serie_Cred_y_Deb.xlsx")
IPC <- read_excel("serie larga IPC.xlsx")
IPC <- IPC %>% select(-c(1,4,5,6))
colnames(IPC)[1] <- "Fecha"
colnames(IPC)[2] <- "IPC"


##mergear ipc y reca
data <- inner_join(Serie_Cred_y_Deb, IPC, by = "Fecha")
data <- data %>%
  mutate(Fecha = as.Date(Fecha, format = "%Y-%m-%d"))


ipc_actual <- tail(data$IPC, 1)

# ajuste por inflacion
data_real <- data %>%
  mutate(Cred_y_Deb_real = Cred_y_Deb * ipc_actual / IPC)


Serie_Cred_y_Deb <- data_real[,-3]
Serie_Cred_y_Deb <- Serie_Cred_y_Deb[,-2]
colnames(Serie_Cred_y_Deb)[2] <- "Cred_y_Deb"

# Crear la serie temporal
ts_Cred_y_Deb <- ts(Serie_Cred_y_Deb$Cred_y_Deb, start = c(2004, 1), frequency = 12)

# Descomponer y desestacionalizar
decomp <- decompose(ts_Cred_y_Deb, type = "multiplicative")
Cred_y_Deb_SIN_EST <- ts_Cred_y_Deb / decomp$seasonal  # Serie desestacionalizada

# Crear columna de fecha
fechas <- seq(as.Date("2004/1/1"), by = "month", length.out = nrow(Serie_Cred_y_Deb))

# Crear el dataframe final con tres columnas: Fecha, Serie Original, Serie Desestacionalizada
Cred_y_Deb_SIN_EST <- Serie_Cred_y_Deb %>%
  mutate(Fecha = fechas, Cred_y_Deb_SIN_EST = as.numeric(Cred_y_Deb_SIN_EST))

# Reorganizar las columnas
Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
  select(Fecha, Cred_y_Deb, Cred_y_Deb_SIN_EST)
colnames(Cred_y_Deb_SIN_EST)[3] <- "Cred_y_Deb_deses"

# Calcular variaci?n interanual
Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
  mutate(Var_Interanual_deses = (Cred_y_Deb_deses / lag(Cred_y_Deb_deses, 12) - 1) * 100)

# Vamos a agregar la suma acumulada trimestral y la variaci?n interanual

Cred_y_Deb_SIN_EST <- Cred_y_Deb_SIN_EST %>%
  arrange(Fecha) %>%
  mutate(Suma_Trimestral = rollapply(Cred_y_Deb_deses, width = 3, FUN = sum, align = "right", fill = NA)) %>%
  mutate(Var_Interanual_Trimestral = (Suma_Trimestral / lag(Suma_Trimestral, 12) - 1) * 100)

C_y_D_interanual <- Cred_y_Deb_SIN_EST %>% 
  select(Fecha, Var_Interanual_deses, Var_Interanual_Trimestral)
  
  
  
  