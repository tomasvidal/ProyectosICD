library(tidyverse)

arbolado <- read.csv("E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_4/arbolado-publico-lineal-2017-2018.csv")

df2 = group_by(arbolado, factor(comuna)) %>%
  mutate(n_arboles = n()) %>% summarise(unique(n_arboles))


### 

comunas = group_by(arbolado, comuna)
comunas2 = mutate(comunas, n_arboles = n(), comuna_fac = factor(comuna)) %>%
  filter(nombre_cientifico == "Jacaranda mimosifolia") %>%
  mutate(n_jacs = n(), prom_jacs = n_jacs/n_arboles) %>%
  group_by(comuna_fac)
  

# jacs = filter(arbolado, nombre_cientifico == "Jacaranda mimosifolia")

resumen = group_by(comunas2, comuna_fac) %>%
  summarise( unique(prom_jacs), unique(n_jacs), unique(n_arboles))

ggplot(comunas2, aes(x=comuna_fac, fill = comuna_fac)) + geom_bar() +
  labs(title = 'Jacarandas por comuna', x='Comuna', y='Cantidad', fill='') #+
#  coord_cartesian(ylim=c(0,40000))

ggplot(comunas, aes(x=factor(comuna), fill = factor(comuna))) + geom_bar() +
  labs(title = 'Arboles por comuna', x='Comuna', y='Cantidad', fill='')

###


filtro_aceras = #filter(arbolado, ancho_acera > 3.5) %>%
  group_by(arbolado, comuna) %>% 
  mutate(ancho = as.numeric(ancho_acera), ancho_prom = mean(ancho, na.rm = T))

res = summarise(filtro_aceras, comuna_f = unique(comuna), ancho = unique(ancho_prom))


grafico = ggplot(res, aes(x=factor(comuna), y=ancho, fill=factor(comuna))) + geom_col() + 
  labs(title='Promedio de ancho de aceras por comuna', x='Comuna', y='Ancho', fill='')
grafico



######


comuna_2 = filter(arbolado, comuna == '2')

# Grafico de los arboles de la comuna 2
grafico_calles = ggplot(comuna_2) +
  geom_jitter(aes(x=long, y=lat)) +
  labs(title='Arboles de la comuna 2') +
  coord_cartesian(ylim=c(-34.60, -34.5825))
grafico_calles


comuna_2_jacarandas = filter(comuna_2, ancho_acera > 3.5, nombre_cientifico == 'Jacaranda mimosifolia') %>%
  group_by(calle_nombre, calle_altura) %>%
  mutate(n_arboles = n()) %>%
  filter(n_arboles < 15, calle_nombre == 'Pueyrredón Av.') %>% 
  summarise(unique(calle_nombre), unique(n_arboles))


comuna_2_calles_2 = filter(comuna_2, ancho_acera > 3.5) %>%
  group_by(calle_nombre, calle_altura) %>%
  mutate(n_arboles = n(), especie = nombre_cientifico, long = long, lat = lat) %>%
  filter(n_arboles < 15, calle_nombre == 'Pueyrredón Av.') #%>% 
  #summarise(unique(calle_nombre), unique(n_arboles))


calle_Av_Puey = filter(comuna_2, calle_nombre == 'Pueyrredón Av.') %>% 
  filter(calle_altura == "1000.0") %>% 
  mutate(calle_altura, prom = mean(altura_arbol))

### 

#Gràfico cantidad de arboles y jacarandàs por cuadra av pueyrredon
df2 <- filter(comuna_2_calles_2, nombre_cientifico=="Jacaranda mimosifolia")
grafico_av_pueyrredon = ggplot() + 
  geom_bar(data=comuna_2_calles_2, aes( x = calle_altura, fill = 'Todas las especies'), width = 0.5, color = 'Black') +
  geom_bar(data=df2, aes( x = calle_altura, fill = 'Jacarandá'), width = 0.5, color = 'Black') +
  labs(title="Cantidad de árboles por cuadra", subtitle = "Av. Pueyrredón - Comuna 2", fill="") +
  xlab("Altura Av. Pueyrredón") +
  ylab("Cantidad de árboles") 
grafico_av_pueyrredon




  
  
  
  
  
  


