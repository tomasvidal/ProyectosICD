library(tidyverse)
library(ggplot2)

glimpse(b_depressed)
unique(b_depressed$education_level)
unique(b_depressed$total_members)

# PHQ_SCORE
# El PHQ-9 es una medida de autoinforme de nueve ítems que evalúa la presencia 
# de síntomas depresivos basados en los criterios del DSM-IV para el episodio depresivo

# GAD_SCORE
# Puntos de corte 0–4 	No se aprecia ansiedad
# 5–9 	Se aprecian síntomas de ansiedad leves
# 10–14 	Se aprecian síntomas de ansiedad moderados
# 15–21 	Se aprecian síntomas de ansiedad severos

# EPWORTH_SCORE
# La Escala o cuestionario de somnolencia diurna de Epworth es un cuestionario​corto tipo Likert 
# que intenta determinar o medir la somnolencia diurna.
# Esto puede ser de ayuda en el diagnóstico de trastornos del sueño. 

unique(depression_anxiety_data$depression_severity)

ggplot(data=b_depressed,aes(x=Age,y=total_members,color=factor(depressed)))+
  geom_point()

ggplot(data=depression_anxiety_data,aes(x=phq_score,y=epworth_score,color=suicidal,shape=anxiousness))+
  geom_point()+ 
  facet_wrap(~gender)

ggplot(data=depression_anxiety_data,aes(x=phq_score,y=gad_score,color=suicidal))+
  geom_point()+ 
  facet_wrap(~gender)

ggplot(data=depression_anxiety_data,aes(x=epworth_score,y=gad_score,color=suicidal,shape=depression_treatment))+
  geom_point()+ 
  facet_wrap(~gender)

ggplot(data=depression_anxiety_data,aes(x=epworth_score,y=bmi,color=suicidal,shape=depressiveness))+
  geom_point()+ 
  facet_wrap(~gender)
