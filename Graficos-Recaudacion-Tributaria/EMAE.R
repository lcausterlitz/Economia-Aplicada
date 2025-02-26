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
EMAE <- read_excel("sh_emae_mensual_base2004.xls") 

EMAE <- EMAE %>% 
  select(,5)
EMAE <- EMAE[-1:-4,]


fechas <- seq(as.Date("2004/1/1"), by = "month", length.out = nrow(EMAE))
EMAE <- cbind(fechas, EMAE)
colnames(EMAE)[1] <- "Fecha"
colnames(EMAE)[2] <- "EMAE_deses"

# Asegurarse de que la columna EMAE sea num?rica
EMAE <- EMAE %>%
  mutate(EMAE_deses = as.numeric(EMAE_deses))

# Ordena los datos por fecha
EMAE <- EMAE %>%
  arrange(Fecha)

# Calcula la suma m?vil trimestral sobre la columna EMAE
EMAE$Suma_Trimestral <- rollapply(EMAE$EMAE_deses, width = 3, FUN = sum, align = "right", fill = NA)

# Calcula la variaci?n interanual de la suma trimestral
EMAE <- EMAE %>%
  mutate(Var_Interanual_Trimestral = (Suma_Trimestral / lag(Suma_Trimestral, 12) - 1) * 100)

# Selecciona las columnas de inter?s
EMAE_interanual <- EMAE %>% 
  select(Fecha, Var_Interanual_Trimestral)


EMAE_variaciones <- EMAE %>%
  mutate(
    Variacion_EMAE = (EMAE_deses / lag(EMAE_deses) - 1) * 100
    ) %>% 
     select(Fecha, Variacion_EMAE)  # Seleccionar solo las columnas deseadas






