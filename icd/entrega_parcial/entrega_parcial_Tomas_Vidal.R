library(tidyverse)
library(nycflights13)

weather_df = read.csv("E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_parcial/weather.csv")
flights_df = flights

weather_df2 = select(weather_df, c(-humid, -dewp, -pressure, -wind_gust, -wind_dir))

flights_df2 = flights_df

flights_df2 = inner_join(flights_df, weather_df2, by= c("year","month","day","hour","origin"))
