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
head(data_renova)
data_solaris <- read.csv("no_anp_solaris_cusco.csv")
head(data_solaris)

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

