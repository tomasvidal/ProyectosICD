library(tidyverse)
library(ggridges)

appear = read_csv(file='E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/appearances.csv'
                  ,locale = readr::locale(encoding = "latin1"))
colnames(appear)

'[1] "gameID"        "playerID"      "goals"         "ownGoals"      "shots"         "xGoals"       
 [7] "xGoalsChain"   "xGoalsBuildup" "assists"       "keyPasses"     "xAssists"      "position"     
[13] "positionOrder" "yellowCard"    "redCard"       "time"          "substituteIn"  "substituteOut"
[19] "leagueID"'


games = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/games.csv')
colnames(games)

' [1] "gameID"          "leagueID"        "season"          "date"            "homeTeamID"     
 [6] "awayTeamID"      "homeGoals"       "awayGoals"       "homeProbability" "drawProbability"
[11] "awayProbability"'


leagues = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/leagues.csv')
colnames(leagues)

'[1] "leagueID"          "name"              "understatNotation"'


players = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/players.csv')
colnames(players)

'[1] "playerID" "name"'


shots = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/shots.csv')
colnames(shots)

' [1] "gameID"     "shooterID"  "assisterID" "minute"     "situation"  "lastAction" "shotType"  
 [8] "shotResult" "xGoal"      "positionX"  "positionY"'


teams = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/teams.csv')
colnames(teams)

'[1] "teamID" "name"'

teamstats = read_csv('E:/Estudios/UNSAM/Carrera/primer cuatri/introduccion a la ciencia de datos/entrega_6/data_frames/teamstats.csv')
colnames(teamstats)

' [1] "gameID"        "teamID"        "season"        "date"          "location"      "goals"        
 [7] "xGoals"        "shots"         "shotsOnTarget" "deep"          "ppda"          "fouls"        
[13] "corners"       "yellowCards"   "redCards"      "result" '


################################


ap_lea = inner_join(appear, leagues, by='leagueID')
premier_stats = filter(ap_lea, understatNotation == 'EPL')

premier_players = inner_join(premier_stats, players, by='playerID') %>% 
  group_by(playerID ) %>% 
  summarise(goles = sum(goals))
ordenado = arrange(premier_players, desc(goles)) %>% head(10)
top_premier_goals = inner_join(ordenado, players, by='playerID')

stats_premier_tops = inner_join(shots, top_premier_goals, by= c("shooterID" = 'playerID')) %>% 
  filter(shotResult == 'Goal')

plot = ggplot(stats_premier_tops, aes(x=minute, y=factor(shooterID), fill=factor(shooterID))) +
  geom_density_ridges(scale=1) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  labs(title='Distribucion de los goles de cada jugador en los 90 minutos de partido',
       subtitle='English Premier League, Seasons 2014-2020',
       x='Minuto',y= 'Jugador', fill= 'Jugador') +
  theme(axis.ticks.y= element_blank(),
        axis.text.y= element_blank()) +
  scale_x_continuous(breaks=seq(0,105,15)) +
  #coord_cartesian(xlim=c(0,100)) +
  scale_fill_hue(labels = (c("Son", "Firmino", "Lukaku", "Sterling", "Aguero",
                            "Kane", "Mahrez", "Vardy", "Mane", "Salah")))
plot


################################


# 1

goles = inner_join(teams, teamstats, by='teamID') 
goles = group_by(goles, name) %>%
  summarise(n_goals = sum(goals))
goles = arrange(goles, -n_goals)

top10_goles = goles[0:10,]


# 2

grafico = ggplot(top10_goles, aes(x=name, y=n_goals, fill=name))+ geom_col()+
  labs(title='TOP 10 equipos con mas goles', subtitle='Ligas Europeas temporadas 2014-2020') +
  xlab('') + ylab('Goles') +
  theme_classic() +
  theme(axis.ticks.x= element_blank(),
        axis.text.x= element_blank(),
        legend.title = element_blank()) +
  coord_cartesian(y=c(400,750)) +
  geom_text(aes(x=name, y=n_goals + 15, label=n_goals))
grafico


# 3

disparos = inner_join(teams, teamstats, by='teamID') 
disparos = group_by(disparos, name) %>%
  summarise(n_shots = sum(shots))
disparos = arrange(disparos, -n_shots)
top10_disparos = disparos[0:10,]



grafico = ggplot(top10_disparos, aes(x=name, y=n_shots, fill=name))+ geom_col()+
  labs(title='TOP 10 equipos con mas disparos', subtitle='Ligas Europeas temporadas 2014-2020') +
  xlab('') + ylab('Disparos') +
  theme_classic() +
  theme(axis.ticks.x= element_blank(),
       axis.text.x= element_blank(),
       legend.title = element_blank()) +
  coord_cartesian(y=c(3000,5000)) +
  geom_text(aes(x=name, y=n_shots + 50, label=n_shots))
grafico


# 4

goles_disparos = left_join(top10_goles, disparos, by='name')

grafico2 = ggplot(goles_disparos) +
  geom_point(aes(x=n_shots, y=n_goals, color= name), size=4)+
  labs(title='Disparos/Goles', subtitle='Equipos top de Europa temporadas 2014-2020') +
  xlab('Disparos') + ylab('Goles') +
  theme(legend.title = element_blank())
grafico2


# 5

todo_europa = left_join(goles, disparos, by='name')

scatter_europa = ggplot(todo_europa) +
  geom_point(aes(x=n_shots, y=n_goals), color='darkmagenta', size = 2) +
  labs(title='Disparos/Goles', subtitle='Equipos de Europa temporadas 2014-2020') +
  xlab('Disparos') + ylab('Goles')

scatter_europa


# 6 y 7

ap_lea = inner_join(appear, leagues, by='leagueID')
premier_stats = filter(ap_lea, understatNotation == 'EPL')

premier_players = inner_join(premier_stats, players, by='playerID') %>% 
  group_by(playerID ) %>% 
  summarise(goles = sum(goals))

premier_players_ordenado = arrange(premier_players, desc(goles)) %>% head(10)
top10_goleadores_premier = inner_join(premier_players_ordenado, players, by='playerID')


# 8

goles_tops_premier = inner_join(shots, top_premier_goals, by= c("shooterID" = 'playerID')) %>% 
  filter(shotResult == 'Goal')
goles_tops_premier$name = as.factor(goles_tops_premier$name)
plot = ggplot(goles_tops_premier, aes(x=minute, y=goles_tops_premier$name)) +
  geom_density_ridges(scale=1) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  labs(title='Distribucion de los goles de cada jugador en los 90 minutos de partido',
       subtitle='English Premier League, Seasons 2014-2020',
       x='Minuto',y= 'Jugador', fill= 'Jugador') +
  # theme(axis.ticks.y= element_blank(),
  #       axis.text.y= element_blank()) +
  scale_x_continuous(breaks=seq(0,105,15)) #+
  # scale_fill_hue(labels = (c("Son", "Firmino", "Lukaku", "Sterling", "Aguero",
                            # "Kane", "Mahrez", "Vardy", "Mane", "Salah")))
plot

plot = ggplot(goles_tops_premier, aes(minute, factor(name))) +
  geom_density_ridges(aes(fill=name)) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 2) +
  labs(title='Distribucion de los goles de cada jugador en los 90 minutos de partido',
       subtitle='English Premier League, Seasons 2014-2020',
       x='Minuto',y= 'Jugador', fill= 'Jugador') #+
  #scale_x_continuous(breaks=seq(0,105,15))

plot

