library(tidyverse)
library(modelr)

arboles = read_csv("/home/tomas/Estudios/R/ProyectosICD/arbolado.csv")

arboles <- group_by(arboles, nombre_cientifico) %>% 
  mutate(n_arboles = n())

summ = summarize(arboles, n = unique(n_arboles)) %>%
  arrange(desc(n)) %>% head(10)
summ <- summ[-9, ]
summ <- summ[-c(1:3), ]

df_arboles = semi_join(arboles, summ, by="nombre_cientifico") %>% 
  mutate(diametro_altura_pecho_mts = diametro_altura_pecho/100)

ggplot(df_arboles,aes(x=diametro_altura_pecho_mts, y=altura_arbol, color=nombre_cientifico)) +
  geom_point()

# Modelo Lineal
arb_mod = lm(altura_arbol ~ diametro_altura_pecho * nombre_cientifico, df_arboles)
df_arboles = add_predictions(arb_mod, data=df_arboles)

ggplot(df_arboles,aes(x=diametro_altura_pecho, y=altura_arbol, color=nombre_cientifico)) +
  geom_point() + geom_line(aes(x=diametro_altura_pecho, y=pred, color=nombre_cientifico))

