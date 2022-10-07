

library(tidyverse)
library(modelr)

# Tarea 1
sim1_mod <- lm(y ~ x, data = sim1)
coef(sim1_mod)

grid <- sim1 %>%
  data_grid(x)
grid <- grid %>%
  add_predictions(sim1_mod)
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = pred), data = grid, colour = "blue", size = 0.5)

sim1 <- sim1 %>%
  add_residuals(sim1_mod)%>%
  mutate(ruido = y + rnorm(1))
ggplot(sim1, aes(x)) +
  geom_point(aes(y = y)) +
  geom_line(aes(y = ruido), data = sim1, colour = "blue", size = 0.5)

ggplot(sim1, aes(resid)) +
  geom_freqpoly(binwidth = 0.5)

view(sim1)
plot(sim1_mod)
summary(sim1_mod)

# Tarea 2

p = ggplot(sim3) +
  geom_point(aes(x=x1, y=y, color=x2))
p

mod1 <- lm(y ~ x1 + x2, data = sim3)
mod2 <- lm(y ~ x1 * x2, data = sim3)

model_matrix(sim3, y ~ x1 + x2)
model_matrix(sim3, y ~ x1 * x2)

grid <- sim3 %>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
ggplot(sim3, aes(x1, y, colour = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~model)

# Tarea 3

view(diamonds)
glimpse(diamonds)

p = ggplot(diamonds) +
  geom_point(aes(x=carat, y=price, color= factor(cut)))
p

model_diamonds = lm(price ~ carat, data=diamonds)
grid_diamonds <- diamonds%>%
  data_grid(carat)
grid_diamonds <- grid_diamonds %>%
  add_predictions(model_diamonds)
p = ggplot(diamonds, aes(x=carat)) +
  geom_point(aes(y = price, color=factor(cut))) +
  geom_line(aes(y = pred), data = grid_diamonds, colour = "red", size = 0.5) +
  coord_cartesian(ylim=c(0,20000))
p

plot(model_diamonds)


gridd2 <- diamonds2%>%
  data_grid(x1, x2) %>%
  gather_predictions(mod1, mod2)
ggplot(sim3, aes(x1, y, colour = x2)) +
  geom_point() +
  geom_line(data = grid, aes(y = pred)) +
  facet_wrap(~model)

model_diamonds2 = lm(log2(price) ~ log2(carat), data = diamonds2)
plot(model_diamonds2)

# Tarea 4

ggplot(data=diamonds, aes(x=factor(cut), y=price)) +
  geom_boxplot()

ggplot(data=diamonds, aes(x=factor(clarity), y=price)) +
  geom_boxplot()

diamonds4 = mutate(diamonds, calidad = case_when(
  cut == "Fair" | cut == 'Good' | cut == 'Very Good' ~ 'malo', cut == "Premium" | cut == 'Ideal' ~ 'bueno')))

