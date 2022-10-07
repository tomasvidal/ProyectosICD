library(tidyverse)
library(modelr)
options(na.action = na.warn)

weather_csv = read_csv("E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/guias/5/datasets/Weather Station Locations.csv")
summary_csv = read_csv("E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/guias/5/datasets/Summary of Weather.csv")

weather = inner_join(weather_csv, summary_csv, by = c("WBAN"="STA"))

ain_station = filter(weather, NAME == 'AIN EL')

weather_model_ain = lm(MaxTemp~MinTemp, ain_station)
ain_station = mutate(add_predictions(ain_station,weather_model_ain))


grafico_temp = ggplot(ain_station, aes(x=MinTemp)) +
  geom_point(aes(y=MaxTemp))+labs(title= 'Temperaturas Máximas y Mínimas', subtitle = 'Durante la segunda guerra mundial') +
  geom_line(aes(y= pred), color='red', size=1)
grafico_temp

# Separo en categorias para obtener mas informacion sobre como afecta la lluvia
# a las temperaturas
ain_station = mutate(ain_station, rain = case_when(
  Precip == "0" ~ "no",
  Precip == "T" ~ "yes",
  TRUE ~ "yes"
))


# 6) 
summary(weather_model_2)
# Grafico la tendencia de cada grupo dependiendo de la lluvia y la temperatura minima
weather_model_ain_2 = lm(MaxTemp ~ MinTemp * rain, ain_station)
grafico = ggplot(ain_station, aes(x = MinTemp, y= MaxTemp, col=rain)) +
  geom_point(size=2.25) +
  geom_abline(aes(slope = 0.97812, intercept = 13.71888, col="no"), size=1.25) +
  geom_abline(aes(slope = 0.97812 + 0.32602, intercept = 13.71888 - 6.71712 , col="yes"), size=1.25) +
  labs(title= 'Temperaturas Máximas y Mínimas', subtitle = 'Estacion AIN EL') 
grafico


# 8)

weather$NAME = as.factor(weather$NAME) # Paso NAME a categorica (era string)

weather_model_all = lm(MaxTemp ~ MinTemp * NAME, weather)

plot_all = ggplot(weather, aes(x=MinTemp, y=MaxTemp)) +
  geom_point(size=2.25) +
  labs(title= 'Temperaturas Máximas y Mínimas', subtitle = 'All stations') +
  geom_line(aes(x=MinTemp, y=MinTemp, col='red'), size=1.25)
plot_all




summary(weather_model_all)

weather = mutate(add_predictions(weather,weather_model_all))
view(weather)

plot_all = ggplot(weather, aes(x=MinTemp, y=MaxTemp)) +
  geom_point(size=2.25) +
  labs(title= 'Temperaturas Máximas y Mínimas', subtitle = 'All stations') +
  geom_line(aes(x=MinTemp, y=MinTemp, col='red'), size=1.25) +
  geom_abline(weather_model_all, aes(col='blue'))
plot_all



