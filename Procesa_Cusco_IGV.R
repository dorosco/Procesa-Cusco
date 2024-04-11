# Procesa datos de Cusco teniendo como input los archivos Excel
# que resultan de haber filtrado las ANP
# 
# **** Incluye la diferenciación de distritos que pertenecen a Ley de Amazonia *****
# ...
# Archivo trabajado en la Desktop

rm(list = ls())
setwd("/Users/davidorosco1/RenovaSolaris/Procesa_Cusco")

library(dplyr)
library(ggplot2)
library(WriteXLS)
library(viridis)

# Lee arcchivos CSV generados previamente desde Excel
data_renova <- read.csv("no_anp_renova_cusco.csv") # Esta data tiene 64k viviendas, verificado que es ORIENTAL
data_solaris <- read.csv("no_anp_solaris_cusco.csv") # Esta data tiene 18k viviendas, verificado que es RENOVA

# Verifica si existen registros cuya region es diferente a Cusco
n_out <- 0
out_cusco_renova <- data_renova %>%
  filter(REGION != "CUSCO")
out_cusco_solaris <- data_solaris %>%
  filter(REGION != "CUSCO")

n_out <- as.integer(out_cusco_solaris %>%
  summarise(n()))
n_out <- n_out + as.integer(out_cusco_renova %>%
  summarise((n())))

if (n_out > 0) {
  print('Alerta:: Existen registros de alguna región diferente')  
}

# Consolidar los datos de ambas fuentes colocando un flag que identifica la fuente

data_1 <- data_solaris %>%
  filter(REGION == "CUSCO") %>%
  select(REGION, PROVINCIA, DISTRITO, LOCALIDAD, LATITUD, LONGITUD) %>%
  mutate(EMPRESA = "RENOVA")

data_2 <- data_renova %>%
  filter(REGION == "CUSCO") %>%
  select(REGION, PROVINCIA, DISTRITO, LOCALIDAD, LATITUD, LONGITUD) %>%
  mutate(EMPRESA = "ORIENTAL")

data_consolidada <- rbind(data_1,data_2)

###########################
# Verifica si hay localidades iguales en ambas empresas

doble_localidad_by_empresa <- data_consolidada %>%
  group_by(EMPRESA, PROVINCIA, DISTRITO, LOCALIDAD) %>%
  summarise(
    q_users = n()
  )

localidades <- doble_localidad_by_empresa %>%
  group_by(PROVINCIA, DISTRITO, LOCALIDAD) %>%
  summarise(
    n_empresa = n_distinct(EMPRESA),
    q_users = sum(q_users)
  )

casos <- localidades %>% # lista de casos de localidades iguales en ambas empresas
  filter(n_empresa > 1)


##########################
# Asigna Flag de pertenencia a Amazonia

data_teste <- data_consolidada %>%
  select(EMPRESA, REGION, PROVINCIA, DISTRITO, LOCALIDAD, LATITUD, LONGITUD) %>%
  mutate(LEYAMAZONIA = FALSE)

teste_ind <- which(data_teste$PROVINCIA ==  "CALCA" & data_teste$DISTRITO == "YANATILE") # primera condicion
data_teste$LEYAMAZONIA[teste_ind] = TRUE

teste_ind <- which(data_teste$PROVINCIA == "LA CONVENCION") # segunda condicion
data_teste$LEYAMAZONIA[teste_ind] = TRUE

teste_ind <- which(data_teste$PROVINCIA == "PAUCARTAMBO" & data_teste$DISTRITO == "KOSÑIPATA") # tercera condicion
data_teste$LEYAMAZONIA[teste_ind] = TRUE

teste_ind <- which(data_teste$PROVINCIA == "QUISPICANCHI" & (data_teste$DISTRITO == "CAMANTI" | data_teste$DISTRITO == "MARCAPATA")) # cuarta condicion
data_teste$LEYAMAZONIA[teste_ind] = TRUE


#########################
# Calcula cantidad de registros en Amazonia

cantidad_div_italia <- sum(data_teste$EMPRESA == "ORIENTAL")

cantidad_div_ley <- sum(data_teste$LEYAMAZONIA)

########################
# Crea tabla de usuarios por empresa y flag de ley

resumo_div_italia <- data_teste %>%
  group_by(REGION, EMPRESA) %>%
  summarise(q_users = n())

resumo_div_ley <- data_teste %>%
  group_by(REGION, LEYAMAZONIA) %>%
  summarise(q_users = n())

WriteXLS(resumo_div_italia, "resumo_div_italia.xlsx")
WriteXLS(resumo_div_ley, "resumo_div_ley.xlsx")

##########################
# Asigna Flag de pertenencia a Oriental para Concesion

data_concesion <- data_teste %>%
  select(EMPRESA, REGION, PROVINCIA, DISTRITO, LOCALIDAD, LATITUD, LONGITUD, LEYAMAZONIA) %>%
  mutate(ENORIENTAL = FALSE)

ind_oriental <- which(data_concesion$LEYAMAZONIA == "TRUE")
data_concesion$ENORIENTAL[ind_oriental] = TRUE

mcantidad <- sum(data_concesion$ENORIENTAL)

# ADICIONAL EL TOTAL DE VIVIENDAS DE LAS PROVINCIAS LIMITROFES CON AMAZONAS" 
ind_oriental <- which(data_concesion$PROVINCIA == "CALCA")
data_concesion$ENORIENTAL[ind_oriental] = TRUE

ind_oriental <- which(data_concesion$PROVINCIA == "PAUCARTAMBO")
data_concesion$ENORIENTAL[ind_oriental] = TRUE

ind_oriental <- which(data_concesion$PROVINCIA == "QUISPICANCHI")
data_concesion$ENORIENTAL[ind_oriental] = TRUE

mcantidad <- sum(data_concesion$ENORIENTAL)
# > 34,155 viviendas

# Calcular la cantidad de usuarios por localidad, distrito y provincia

users_by_localidad <- data_concesion %>%
  group_by(PROVINCIA, DISTRITO, LOCALIDAD) %>%
  summarise(
    q_users = n()
  )

users_by_distrito <- users_by_localidad %>%
  group_by(PROVINCIA, DISTRITO) %>%
  summarise(
    q_users = sum(q_users)
  )

users_by_provincia <- users_by_distrito %>%
  group_by(PROVINCIA) %>%
  summarise(
    q_users = sum(q_users)
  )

WriteXLS(users_by_provincia, "users_by_provincia.xlsx")
WriteXLS(users_by_distrito, "users_by_distrito.xlsx")
WriteXLS(users_by_localidad, "users_by_localidad.xlsx")
