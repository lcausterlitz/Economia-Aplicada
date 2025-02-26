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
library(purrr)


setwd("C:/Users/DELL/OneDrive/Desktop/MECON/ENGHO")
engho_gastos_combu <- read.table("engho2018_gastos.txt", header = TRUE, sep ="|")
engho_articulos <- read.table("engho2018_articulos.txt", header = TRUE, sep ="|")
engho_ingresos_combu <- read.table("engho2018_hogares.txt", header = TRUE, sep ="|")

engho_articulos_combus <- engho_articulos %>%
  select(articulo, articulo_desc) %>%
  filter(articulo_desc %in% c("Otros combustibles para el hogar",
                              "Otros gastos en combustibles",
                              "Calefon, termotanque a otros combustibles",
                              "Gas envasado en garrrafas",
                              "Gas a granel",
                              "Gas natural por red domiciliaria (m3) de la vivienda principal",
                              "Gas envasado en tubo",
                              "Gas natural por red domiciliaria (m3) de la vivienda secundaria",
                              "Gastos de  gas realizados para una vivienda ocupada por otro hogar",
                              "Diesel, gasoil",
                              "Diesel premium",
                              "Gas natural comprimido, GNC",
                              "Nafta súper",
                              "Nafta premium"))

articulos_combus <- as.data.frame(engho_articulos_combus$articulo)
# list(articulos_combus)

#ya tengo los articulos de combustibles, falta calcular el gasto total

engho_gastos_combu <- engho_gastos_combu %>%
  filter(articulo %in% c( "A0452101",
                          "A0452102",
                          "A0452103",
                          "A0452104",
                          "A0452105",
                          "A0452106",
                          "A0453103",
                          "A0722201",
                          "A0722202",
                          "A0722301",
                          "A0722401",
                          "A0722102",
                          "A0722103"))

engho_gastos_combu$gasto_combu <- engho_gastos_combu$monto * engho_gastos_combu$pondera

engho_gastos_combu$descripcion_combu <- "a"
engho_gastos_combu <- engho_gastos_combu %>%
  mutate(descripcion_combu = case_when(
    articulo == "A0452101" ~ "Gas envasado en garrrafas",
    articulo == "A0452102" ~ "Gas a granel",
    articulo == "A0452103" ~ "Gas natural por red domiciliaria (m3) de la vivienda principal",
    articulo == "A0452104" ~ "Gas envasado en tubo",
    articulo == "A0452105" ~ "Gas natural por red domiciliaria (m3) de la vivienda secundar",
    articulo == "A0452106" ~ "Gastos de  gas realizados para una vivienda ocupada por ot",
    articulo == "A0453103" ~ "Otros combustibles para el hogar",
    articulo == "A0722201" ~ "Diesel, gasoil",
    articulo == "A0722202" ~ "Diesel premium",
    articulo == "A0722301" ~ "Gas natural comprimido, GNC",
    articulo == "A0722102" ~ "Nafta súper",
    articulo == "A0722103" ~ "Nafta premium"
  ))

engho_gastos_combu$suma_fija <- 1

###############
#prueba para nafta suma fijas marzo-2018 

engho_nafta <- engho_gastos_combu %>% 
  filter(descripcion_combu %in% c("Nafta súper", "Nafta premium"))
engho_nafta$suma_fija <- 6.726 + 0.412 #nafta y co2 nafta
engho_nafta$cantidad <- engho_nafta$cantidad * engho_nafta$pondera

#calculo pago de ICL naftas
engho_nafta$ICL <- engho_nafta$cantidad * engho_nafta$suma_fija
icl_nafta <- sum(engho_nafta$ICL)
gasto_nafta <- sum(engho_nafta$gasto_combu)

#presion ICL naftas ~ 0,8% (falta quitar zona sur)
ingresos <- sum(engho_ingresos_combu$ingtoth * engho_ingresos_combu$pondera)
presion_icl_nafta <- icl_nafta / ingresos

#############
#prueba para gasoil
engho_gasoil <- engho_gastos_combu %>% 
  filter(descripcion_combu %in% c("Diesel, gasoil", "Diesel premium"))
engho_gasoil$suma_fija <- 4.148 + 0.473 #gasoil y co2 gasoil
engho_gasoil$cantidad <- engho_gasoil$cantidad * engho_gasoil$pondera

#calculo pago de ICL gasoil
engho_gasoil$ICL <- engho_gasoil$cantidad * engho_gasoil$suma_fija
icl_gasoil <- sum(engho_gasoil$ICL)
gasto_gasoil <- sum(engho_gasoil$gasto_combu)

#presion ICL gasoil ~ 0,1% (falta quitar zona sur)
presion_icl_gasoil <- icl_gasoil / ingresos

############

#prueba para Recargo Consumo de Gas
engho_gas <- engho_gastos_combu %>% 
  filter(descripcion_combu %in% c("Gas natural por red domiciliaria (m3) de la vivienda principal", "Gas natural por red domiciliaria (m3) de la vivienda secundaria"))
engho_gas$alicuota <-  .0296
engho_gas$cantidad <- engho_gas$cantidad * engho_gas$pondera

#calculo pago de Recargo Consumo de Gas
engho_gas$recargo <- engho_gas$gasto_combu * engho_gas$alicuota
recargo_gas <- sum(engho_gas$recargo)
gasto_gas <- sum(engho_gas$gasto_combu)

#presion Recargo Consumo de Gas ~ 0,038% (falta quitar zona sur)
presion_gas <- recargo_gas / ingresos

#############
porcentaje_gasto_nafta <- gasto_nafta / ingresos # ~ 3,9%
porcentaje_gasto_combus <- sum(engho_gastos_combu$gasto_combu) / ingresos # ~ 6,7%
porcentaje_gasto_gasoil <- gasto_gasoil / ingresos # ~ 0,7%
porcentaje_gasto_gas <- gasto_gas / ingresos # ~ 1,2%

#######prendientes#######
#quitar zona sur
#mergear con base hogares por id para recalcular por decil
#filtrar 2018 desde segundo trimestre

#####mergeo bases para nafta, gasoil y gas y calculo presion por decil y %gasto
engho_ingresos_combu$ingresos_totales <- engho_ingresos_combu$ingtoth * engho_ingresos_combu$pondera
engho_ingresos_combu$gasto_total <- engho_ingresos_combu$gastot * engho_ingresos_combu$pondera

#nafta
in_nafta <- left_join(engho_nafta, engho_ingresos_combu, by ="id")
in_nafta_2 <- in_nafta %>% 
  select(id, ICL, dinpch_t, ingresos_totales, gasto_total)


nafta_decil <- data.frame(decil = numeric(0), presion = numeric(0))
decil <- 1:10

for (i in decil) {
  base_nafta <- in_nafta_2 %>%
    filter(dinpch_t == i)
  icl <- sum(base_nafta$ICL)
  ingreso_decil <- engho_ingresos_combu %>% 
    filter(dinpch_t == i)
  ingreso <- sum(ingreso_decil$ingresos_totales)
  presion <- icl / ingreso
  
  nafta_decil <- rbind(nafta_decil, data.frame(decil = i, presion = presion))  
}
colnames(nafta_decil)[2] <- "presion_naftas"

#ingresos en in_nafta_2 no da igual q engho_ingreso 
#porque no todos los hogares consumen combustible

#gasoil
in_gasoil <- left_join(engho_gasoil, engho_ingresos_combu, by ="id")
in_gasoil_2 <- in_gasoil %>% 
  select(id, ICL, dinpch_t, ingresos_totales, gasto_total)


gasoil_decil <- data.frame(decil = numeric(0), presion = numeric(0))
decil <- 1:10

for (i in decil) {
  base_gasoil <- in_gasoil_2 %>%
    filter(dinpch_t == i)
  icl <- sum(base_gasoil$ICL)
  ingreso_decil <- engho_ingresos_combu %>% 
    filter(dinpch_t == i)
  ingreso <- sum(ingreso_decil$ingresos_totales)
  presion <- icl / ingreso
  
  gasoil_decil <- rbind(gasoil_decil, data.frame(decil = i, presion = presion))  
}

colnames(gasoil_decil)[2] <- "presion_gasoil"

#Recacrgo consumo de gas
in_gas <- left_join(engho_gas, engho_ingresos_combu, by ="id")
in_gas_2 <- in_gas %>% 
  select(id, recargo, dinpch_t, ingresos_totales, gasto_total)


gas_decil <- data.frame(decil = numeric(0), presion = numeric(0))
decil <- 1:10

for (i in decil) {
  base_gas <- in_gas_2 %>%
    filter(dinpch_t == i)
  recargo_consumo_gas <- sum(base_gas$recargo)
  ingreso_decil <- engho_ingresos_combu %>% 
    filter(dinpch_t == i)
  ingreso <- sum(ingreso_decil$ingresos_totales)
  presion <- recargo_consumo_gas / ingreso
  
  gas_decil <- rbind(gas_decil, data.frame(decil = i, presion = presion))  
}

colnames(gas_decil)[2] <- "presion_recargo_gas"

#agrupo dataframes

# Lista de dataframes a combinar
dataframes <- list(nafta_decil, gas_decil, gasoil_decil)

# Merge usando reduce
presion_total <- reduce(dataframes, left_join, by = "decil")

#hay mayor gasto en nafta y mas ICL, por lo tanto mayor presion

write.xlsx(presion_total, "base_prueba_presion_combustibles.xlsx")
                      



