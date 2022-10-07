library(tidyverse)
library(nycflights13)
library(forcats)
library(ggridges)

flights_df = mutate(flights, time_rec = dep_delay - arr_delay) %>%
  group_by(month)

flights_summ = summarise(flights_df, dep_delay, time_rec)

flights_df2 = mutate(flights_df, mes = factor(month))


ggplot(data=flights_df2) + geom_histogram(aes(x=dep_delay, fill=mes),
                                          binwidth = 7.5,
                                          position = "dodge") + xlim=c(0, 100)


# plot_delay = ggplot(flights_df2, aes(x = dep_delay , y = mes, fill = mes )) +
#   geom_density_ridges() +
#   stat_density_ridges(quantile_lines = TRUE) +
#   lims(x = c(-20, 20)) +
#   labs(title = 'Distribucion de retraso en las salidas por mes',
#        x='Retraso', y = 'Mes',
#        subtitle = 'Vuelos de 2013')
# plot_delay
# 
# plot_time_rec = ggplot(flights_df2, aes(x = time_rec , y = mes, fill = factor(mes))) +
#   geom_density_ridges(scale = 1) +
#   stat_density_ridges(quantile_lines = TRUE) +
#   lims(x = c(-30, 40)) +
#   labs(title = 'Distribucion de tiempo recuperado en el aire por mes',
#        x='Tiempo recuperado', y = 'Mes',
#        subtitle = 'Vuelos de 2013')
# plot_time_rec


# Observo cuantos vuelos tiene cada aerolinea
flights_aer = ungroup(flights_df2) %>% group_by(carrier)
flights_aer = summarise(flights_aer, n_vuelos = n())
flights_aer

# Elijo la aerolinea EV
flights_EV = filter(flights_df2, carrier == 'EV')
flights_EV

# Calculo la media, mediana sd e IQR por mes del retraso
flights_EV = group_by(flights_EV, mes)
flights_EV = mutate(flights_EV, mediana_del = median(dep_delay, na.rm = TRUE))

# Calculo la media, mediana sd e IQR por mes del tiempo recuperado
flights_EV = group_by(flights_EV, mes)
flights_EV = mutate(flights_EV, mediana_time_rec = median(time_rec, na.rm = TRUE))




# Density ridges delay con negativos
plot_delay = ggplot(flights_EV, aes(x = dep_delay , y = mes, fill = mes )) +
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE) +
  coord_cartesian(xlim= c(-20, 50)) +
  labs(title = 'Distribucion de retraso en las salidas por mes', 
       x='Retraso [min]', y = 'Mes',
       subtitle = 'Vuelos de 2013, arolinea EV') + 
  geom_vline(aes(xintercept = 0))
plot_delay


# Density ridges retraso literal (dep_delay > 0)
plot_delay_0 = ggplot(flights_EV, aes(x = dep_delay , y = mes, fill = mes )) +
  geom_density_ridges() +
  stat_density_ridges(quantile_lines = TRUE) +
  coord_cartesian(xlim= c(0, 50)) +
  labs(title = 'Distribucion de retraso en las salidas por mes', 
       x='Retraso [min]', y = 'Mes',
       subtitle = 'Vuelos de 2013, arolinea EV') + 
  geom_vline(aes(xintercept = 0))
plot_delay_0


# Density ridges tiempo recuperado
plot_time_rec = ggplot(flights_EV, aes(x = time_rec , y = mes, fill = factor(mes))) +
  geom_density_ridges(scale = 1) +
  stat_density_ridges(quantile_lines = TRUE) +
  coord_cartesian(xlim= c(-30, 40)) +
  labs(title = 'Distribucion de tiempo recuperado en el aire por mes', 
       x='Tiempo recuperado [min]', y = 'Mes',
       subtitle = 'Vuelos de 2013, aerolinea EV')

plot_time_rec

labels = labs(title = 'Distribucion de retraso en las salidas por mes')
####

# geom_freqpoly con los mismos datos que el geom_density pero no me gusta mucho la verdad
# plot_delay_freq = ggplot(flights_EV, aes(x = dep_delay, colour = mes )) + geom_freqpoly(binwidth = 10) +
#   coord_cartesian(xlim= c(-20, 20)) 
#   #labs(title = 'Distribucion de retraso en las salidas por mes', 
#    #    x='Retraso [min]', y = 'Mes',
#     #   subtitle = 'Vuelos de 2013, arolinea EV') #+ 
# #  geom_vline(aes(xintercept = 0, colour = 'On time'))
# plot_delay_freq



####



# Boxplot x=delay, y=mes
plot_delay_boxplot = ggplot(flights_EV, aes(x = dep_delay, y = mes , color = mes)) +
  coord_cartesian(xlim= c(-10, 100)) +
  stat_boxplot(geom='errorbar', width=0.25) +
  labs(title = 'Distribucion de retraso en las salidas por mes',
       subtitle = 'Vuelos de la aerolinea EV en 2013', x = 'Retraso [min]', y = 'Mes') + 
  geom_vline(aes(xintercept = 0)) +
  geom_boxplot()

plot_delay_boxplot

# Boxplot x=mes y=delay
plot_delay_boxplot = ggplot(flights_EV, aes(x = mes, y = dep_delay, color = mes)) +
  coord_cartesian(ylim= c(-10, 40)) +
  stat_boxplot(geom='errorbar', width=0.25) +
  labs(title = 'Distribucion de retraso en las salidas por mes',
       subtitle = 'Vuelos de la aerolinea EV en 2013', x = 'Mes', y = 'Retraso [min]') + 
  geom_hline(aes(yintercept = 0)) +
  geom_boxplot()

plot_delay_boxplot


# Observar diciembre y marzo
