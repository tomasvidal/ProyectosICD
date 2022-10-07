library(tidyverse)
library(modelr)
library(nycflights13)


df_flights <- flights

# Elijo 3 aerolineas random
df_flights = filter(df_flights, carrier == c("F9","FL","EV"))

# Armo el modelo
mod_flights = lm(arr_delay ~ dep_delay + carrier, data = df_flights)
mod_flights2 = lm(arr_delay ~ dep_delay, data = df_flights)

grid = df_flights %>% 
  data_grid(dep_delay) %>% 
  add_predictions(mod_flights)

# Grafico las predicciones
ggplot(df_flights, aes(x=dep_delay)) +
  geom_point(aes(y=arr_delay, color=carrier)) + 
  geom_line(data=grid ,aes(y=pred)) +
  facet_wrap(~carrier) +
  labs(title="Retraso en la salida en funcion del retraso en la llegada",
       subtitle="Aerolineas EV,F9,FL",
       x="Retraso en la salida",
       y="Retraso en la llegada")


summary(mod_flights)
summary(mod_flights2)

mod_flights3 = lm(arr_delay ~ dep_delay * hour + carrier, data = df_flights)

df_flights3 = add_predictions(df_flights, mod_flights3)

ggplot(df_flights3, aes(x=dep_delay)) +
  geom_point(aes(y=arr_delay, color=carrier)) + 
  geom_line(data=df_flights3 ,aes(y=pred)) +
  facet_wrap(~carrier) +
  labs(title="Retraso en la salida en funcion del retraso en la llegada",
       subtitle="Aerolineas EV,F9,FL",
       x="Retraso en la salida",
       y="Retraso en la llegada")
model_matrix(df_flights3, mod_flights3)


