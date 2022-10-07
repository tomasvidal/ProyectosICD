library(tidyverse)
library(nycflights13)
library(forcats)

print(flights)
view(flights)

# Mutate orden ascendente
flights_m = mutate(flights, origin=fct_rev(fct_infreq(origin)))

# Grafico
ggplot(data=flights_m) + geom_bar(aes(x=origin), fill='Red', width=0.5)                                

# Filter
flights_f = filter(flights_m, dep_delay > 60)

# Grafico
ggplot(data=flights_f) + geom_bar(aes(x=origin), fill='Yellow', width=0.5)

# Agrupar
flights_mg = group_by(flights_m, origin)

# Summarise/resumen
flights_mgs = summarise(flights_mg, n_vuelos=n(), n_del=sum(dep_delay>60, na.rm=T), frac_del=n_del/n_vuelos * 100)

plot = ggplot(data=flights_mgs) + geom_bar(aes(x=origin, y=frac_del), fill='Green', width=0.5, stat='identity')
  
plot = plot + labs(title= 'Fraccion de vuelos retrasados por aeropuerto de origen', subtitle= 'Vuelos de 2013',
                    x= 'Aeropuerto de origen', y= 'Porcentaje de vuelos retrasados') 
plot


###

flights_mg = group_by(flights_m, origin, carrier)

flights_mgs = summarise(flights_mg, n_vuelos=n(), n_del=sum(dep_delay>60, na.rm=T), frac_del=n_del/n_vuelos * 100)

plot = ggplot(data=flights_mgs) + geom_bar(aes(x=carrier, y=frac_del, fill=origin), width=0.5, stat='identity')
plot = plot + labs(title= 'Fraccion de vuelos retrasados por aerolinea en cada aeropuuerto',
                   subtitle= 'Vuelos de 2013', x= 'Aerolinea', y='Porcentaje de vuelos retrasados')
plot

plot2 = plot + ggplot(data=flights_mgs) + geom_bar(aes(x=carrier, y=frac_del, fill=origin),
                                                   width=0.5,
                                                   stat='identity',
                                                   position= position_dodge(width=0.7, preserve = 'single'))
plot2 = plot2 + labs(title= 'Fraccion de vuelos retrasados por aerolinea en cada aeropuuerto',
                     subtitle= 'Vuelos de 2013', x= 'Aerolinea', y='Porcentaje de vuelos retrasados')
plot2















