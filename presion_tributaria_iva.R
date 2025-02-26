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
    subclase %in% c("A01112", "A01121", "A01123", "A01122", "A01161", "A01171") ~ .105,
    grupo == "A073" ~ .105,
    grupo %in% c("A094","A061") ~ 0,
    division == "A10" ~ 0,
    clase %in% c("A0951", "A0952", "A0954") ~ .105,
    subclase %in% c("A01111", "A01141", "A04111") ~ 0,
    TRUE ~ iva_alicuota
  ))

########################################
# Calcular el IVA pagado
########################################

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

##################################
#Calculo el iva pagado en cada division de bienes por decil
########################################

# Crear un dataframe vacío para almacenar el IVA pagado por decil y división

iva_decil_division <- data.frame(decil = 1:10)
# Obtener las divisiones únicas de bienes
divisiones <- c("A01", "A02","A03","A04","A05","A06","A07","A08","A09","A10","A11","A12")
# Bucle for para calcular el IVA pagado por decil y división
for (div in divisiones) {
  iva_division <- c()  # Crear vector temporal para almacenar IVA por decil en esta división
  
  for (i in 1:10) {
    base_iva <- engho_gastos %>%
      filter(dinpch_t == i, division == div)
    
    # Sumar el IVA pagado para el decil en esta división
    iva <- sum(base_iva$iva, na.rm = TRUE)
    iva_division <- c(iva_division, iva)
  }
  
  # Añadir la columna de esta división al dataframe
  iva_decil_division[[div]] <- iva_division
}

# Sumar los valores de todas las columnas excepto 'decil'
iva_decil_division$total_iva <- rowSums(iva_decil_division[ , -1], na.rm = TRUE)

# Mostrar el resultado
print(iva_decil_division)

#chequeo que el iva total aperturado por division coincida con el valor calculado sin distinguir la division
#debo correr codigo engho_presion_iva_total
chequeo_iva <- as.data.frame(base_presion_decil$iva - iva_decil_division$total_iva)

########################################
#calculo los ingresos por decil con la engho_hogares
########################################

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

########################################
#cruzo las bases para calcular la presion por decil
########################################

base_presion_decil <- left_join(iva_decil, ingreso_decil, by = "decil")
base_presion_decil$presion <- base_presion_decil$iva / base_presion_decil$ingreso
print(base_presion_decil)
write.xlsx(base_presion_decil, "base_presion_decil.xlsx")

########################################
#calculo la presion para cada division de bienes por decil
########################################

base_presion_division <- left_join(iva_decil_division, ingreso_decil, by = "decil")

# Bucle para calcular la presión por cada división y agregarla como una nueva columna
for (div in divisiones) {
  
  # Crear el nombre dinámico de la columna para la presión de cada división
  nombre_columna <- paste0("presion_", div)
  
  # Calcular la presión por división y agregarla como una nueva columna
  base_presion_division <- base_presion_division %>%
    mutate(!!sym(nombre_columna) := get(div) / ingreso)
}

# Mostrar el resultado
print(base_presion_division)

########################################
#calculo la participacion de cada division en el iva
########################################

for (div in divisiones) {
  
  # Crear el nombre dinámico de la columna para la participacion en el iva de cada división
  nombre_columna <- paste0("participacion_", div)
  
  # Calcular la participacion por división y agregarla como una nueva columna
  base_presion_division <- base_presion_division %>%
    mutate(!!sym(nombre_columna) := get(div) / total_iva)
}

########################################
#traigo la columna de presion total y exporto el excel final
########################################
base_presion_division <- base_presion_division %>%
  mutate(presion_total = base_presion_decil$presion)

write.xlsx(base_presion_division, "base_final_presion_iva.xlsx")
