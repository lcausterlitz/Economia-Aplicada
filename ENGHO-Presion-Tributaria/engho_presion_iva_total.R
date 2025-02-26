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



setwd("C:/Users/DELL/OneDrive/Desktop/MECON/ENGHO")

#Operamos con la engho_gastos

engho_gastos <- read.table("engho2018_gastos.txt", header = TRUE, sep ="|")

# Definir las alícuotas de IVA
engho_gastos$iva_alicuota <- .21

engho_gastos <- engho_gastos  %>%
  mutate(iva_alicuota = case_when(
    provincia == 94 ~ 0,
    subclase %in% c("A01112", "A01121", "A01123", "A01122", "A01161", "A01171") ~ .105,
    grupo == "A073" ~ .105,
    grupo %in% c("A094","A061") ~ 0,
    division == "A10" ~ 0,
    clase %in% c("A0951", "A0952", "A0954") ~ .105,
    subclase %in% c("A01111", "A01141", "A04111") ~ 0,
    TRUE ~ iva_alicuota
  ))

# Calcular el IVA pagado
engho_gastos$precio_neto_iva <- engho_gastos$monto / (engho_gastos$iva_alicuota + 1)
engho_gastos$iva_pagado <- engho_gastos$monto - engho_gastos$precio_neto_iva 

#pondero el iva
engho_gastos$iva <- engho_gastos$iva_pagado * engho_gastos$pondera

#traigo los deciles de la base hogares
engho_hogares <- read.table("engho2018_hogares.txt", header = TRUE, sep ="|")
engho_hogares_deciles <- engho_hogares %>%
  select(id, dinpch_t)
engho_gastos <- left_join(engho_gastos, engho_hogares_deciles, by = "id")

#calculo iva pagado por decil
iva_decil <- data.frame(decil = numeric(0), iva = numeric(0))
decil <- 1:10

for (i in decil) {
  base_iva <- engho_gastos %>%
    filter(dinpch_t == i)
  iva <- sum(base_iva$iva)
    
  iva_decil <- rbind(iva_decil, data.frame(decil = i, iva = iva))  
}


#calculo los ingresos por decil con la engho_hogares
  #primero pondero los ingresos
engho_hogares$ingreso <- engho_hogares$ingtoth * engho_hogares$pondera

#creo un dataframe para almacenar los datos
ingreso_decil <- data.frame(decil = numeric(0), ingreso = numeric(0))

for (i in decil) {
  base_ingreso <- engho_hogares %>%
    filter(dinpch_t == i)
  ingreso <- sum(base_ingreso$ingreso)
  
  ingreso_decil <- rbind(ingreso_decil, data.frame(decil = i, ingreso = ingreso))  
}

#cruzo las bases para calcular la presion por decil
base_presion_decil <- left_join(iva_decil, ingreso_decil, by = "decil")
base_presion_decil$presion <- base_presion_decil$iva / base_presion_decil$ingreso
print(base_presion_decil)
write.xlsx(base_presion_decil, "base_presion_decil.xlsx")



































