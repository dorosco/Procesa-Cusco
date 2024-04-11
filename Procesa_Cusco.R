# Procesa datos de Cusco teniendo como input los archivos Excel
# que resultan de haber filtrado las ANP

rm(list = ls())
setwd("/Users/davidorosco1/RenovaSolaris/Procesa_Cusco")

library(dplyr)
library(ggplot2)
library(WriteXLS)
library(viridis)

# Lee arcchivos CSV generados previamente desde Excel
data_renova <- read.csv("no_anp_renova_cusco.csv")
data_solaris <- read.csv("no_anp_solaris_cusco.csv")

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


# Calcular la cantidad de usuarios por localidad, distrito y provincia

users_by_localidad <- data_consolidada %>%
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

cantidad_usuarios <- sum(users_by_provincia$q_users)


localidades_ordered <- users_by_localidad %>%
  arrange(PROVINCIA,DISTRITO,LOCALIDAD,.by_group = TRUE) %>%
  select(PROVINCIA, DISTRITO, LOCALIDAD)

localidades_v1 <- read.csv("localidades_cusco_v1.csv")
localidades_v1 <- localidades_v1 %>%
  select(PROVINCIA, DISTRITO, LOCALIDAD)

en_ambos <- intersect(localidades_ordered, localidades_v1)

solo_en_final <- setdiff(localidades_ordered,localidades_v1)

excluidas <- setdiff(localidades_v1, localidades_ordered)

# Genera los gráficos BoxPlot 

users_by_localidad %>%
  ggplot(aes(x=PROVINCIA, y=q_users)) +
  geom_boxplot(outlier.shape = NA) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  coord_cartesian(ylim =  c(0, 60)) +
  annotate("text",
           x = 1:length(table(users_by_localidad$PROVINCIA)),
           y = aggregate(q_users ~ PROVINCIA, users_by_localidad, median)[,2],
           label = table(users_by_localidad$PROVINCIA),
           col = "red",
           vjust = -1)

users_by_localidad %>%
  ggplot( aes(x=PROVINCIA, y=q_users, fill=PROVINCIA)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.6) +
  #theme_ipsum() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
    legend.position="none"
#    plot.title = element_text(size=11)
  ) +
  coord_cartesian(ylim =  c(0, 60)) +
  annotate("text",
           x = 1:length(table(users_by_localidad$PROVINCIA)),
#           y = aggregate(q_users ~ PROVINCIA, users_by_localidad, median)[,2],
           y = 55,
           label = paste("n =",table(users_by_localidad$PROVINCIA)),
           col = "red",
           size = 3,
           vjust = -1) +
  ggtitle("BoxPlot de Cantidad de Viviendas por Localidad en Provincias de Cusco") +
  xlab("PROVINCIA") +
  ylab("# de viviendas")

WriteXLS(users_by_localidad, "out_localidades_cusco.xlsx")
