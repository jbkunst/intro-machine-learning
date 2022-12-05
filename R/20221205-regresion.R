# https://setosa.io/ev/ordinary-least-squares-regression/
library(tidyverse)

theme_set(theme_minimal())

anscombe

sets <- c("Normal (x1, y1)", 
          "Falta de Ajuste (x2, y2)",
          "Outiler (x3, y3)", 
          "Influencia (x4, y4)")

dfanscombe <- as_tibble(anscombe) |> 
  mutate(observation = row_number()) |> 
  gather(key, value, -observation) |> 
  separate(key, c("variable", "set"), 1, convert = TRUE) |> 
  mutate(set = factor(set, labels = sets)) |> 
  spread(variable, value) |> 
  select(set, observation, x, y) |> 
  arrange(set, observation) |> 
  mutate(setnum = as.numeric(set), .after = set)

glimpse(dfanscombe)

dfanscombe

dfanscombe |> 
  group_by(set) |> 
  summarise(
    mean_x = mean(x),
    mean_y = mean(y),
    sd_x   = sd(x),
    sd_y   = sd(y),
    cor_xy = cor(x , y)
  )

ggplot(dfanscombe, aes(x = x, y = y)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  geom_smooth(method = "lm", color = "navy", alpha = 0.1, formula = 'y ~ x', se = FALSE) +
  facet_wrap(vars(set), scales = "fixed") +
  labs(x = NULL, y = NULL)

filter(dfanscombe, set == "Influencia (x4, y4)")

filter(dfanscombe, setnum == 4)

ggplot(filter(dfanscombe, setnum == 4), aes(x = x, y = y)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  geom_smooth(method = "lm", color = "navy", alpha = 0.1) +
  facet_wrap(vars(set), scales = "fixed") +
  labs(x = NULL, y = NULL)


lm1 <- lm(y ~ x, data = filter(dfanscombe, setnum == 1))
lm1

lm2 <- lm(y ~ x, data = filter(dfanscombe, setnum == 2))
lm2

lm3 <- lm(y ~ x, data = filter(dfanscombe, setnum == 3))
lm3

lm4 <- lm(y ~ x, data = filter(dfanscombe, setnum == 4))
lm4

# dos formas de obtener los residuos e_i
lm1$residuals
filter(dfanscombe, setnum == 1)$y - predict(lm1)

#' Evaluacion de calidad del modelo estudiando errores (residuos)
dfanscombe <- dfanscombe |>
  mutate(
    res = c(lm1$residuals, lm2$residuals, lm3$residuals, lm4$residuals)
  )

dfanscombe

ggplot(dfanscombe, aes(x = x, y = res)) +
  geom_point(color = "darkred", size = 4, shape = 1) +
  # geom_smooth(method = "lm", color = "navy", alpha = 0.1) +
  geom_smooth(method = "loess", color = "skyblue", alpha = 0.1, span = 1, se = FALSE) +
  facet_wrap(vars(set), scales = "fixed") +
  labs(x = NULL, y = NULL)


# transformaciones --------------------------------------------------------
df <- tibble(
  x = 10 + 1:50,
  y = 1 + scales::rescale(10 +  x^2 + rnorm(length(x), sd = 25) + 10)
)

df

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1)

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

mod <- lm(y ~ x, data = df)

df <- mutate(df, er = as.vector(mod$residuals))

ggplot(df, aes(x, er)) + 
  geom_point() +
  geom_smooth()

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, log(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

mod2 <- lm(log(y) ~ x, data = df)

mod2

ggplot(df, aes(x, er2)) + 
  geom_point() +
  geom_smooth()

df <- mutate(df, er2 = as.vector(mod$residuals))

ggplot(df, aes(sqrt(x), y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

mod2 <- lm(sqrt(y) ~ x, data = df)

df <- mutate(df, er2 = mod2$residuals)

ggplot(df) + 
  geom_point(aes(x, er2))


# heterocedasticidad
df <- data_frame(
  x = 1:100,
  y = 50 +  10 * x + (10 + x^1.2) * rnorm(length(x), sd = 1)
)

df

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1)

ggplot(df, aes(x, y)) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, log(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")

ggplot(df, aes(x, sqrt(y))) + 
  geom_point(shape = 1) +
  geom_smooth(method = "lm")


plot(AirPassengers)

plot(log(AirPassengers))

