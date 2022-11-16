library(tidyverse)

# parte matematica --------------------------------------------------------
df <- tibble(x = seq(-5, 5, by = 0.1))

ggplot(df,aes(x)) +
  stat_function(fun = sqrt)

ggplot(df,aes(x)) +
  stat_function(fun = sin)

# fun=function(x) log(x)/x
fun <- function(x){ 
  log(x)/x
  }

ggplot(df,aes(x)) +
  stat_function(fun = fun)



# fun=function(x) log(x)/x
fun1 <- function(x){ 
  1/(1+exp(-x))
}

fun1(10)
fun1(-10)

fun1(0)
fun1(10) + fun1(-10)

fun2 <- function(x){ 
  1/(1+exp(-(8+5*x)))
}

ggplot(df,aes(x)) +
  stat_function(fun = fun1) +
  stat_function(fun = fun2, color = "red") +
  scale_x_continuous(breaks = -5:5)


 -8/5

# parte datos -------------------------------------------------------------
ruta <- "https://raw.githubusercontent.com/jadeyee/r2d3-part-1-data/master/part_1_data.csv"

datos <- readr::read_csv(ruta, skip = 2)

dmin <- datos |> 
  select(elevation, in_sf)

glimpse(datos)

df <- tibble(x = seq(0, 250, by = 0.1))

funintuicion <- function(x){
  1/(1+exp(-( -30 + 1*x )))
}

ggplot(dmin) +
  geom_point(aes(elevation, in_sf), color = "gray60", alpha = 0.2, size = 3) +
  stat_function(fun = funintuicion, data = df, color = "darkred") +
  theme_minimal()

dmin <- dmin |> 
  mutate(
    ypred = funintuicion(elevation),
    error = in_sf - ypred
    ) 

dmin |>
  summarise(rmse = sqrt(mean(error^2)))
  
  
ggplot(dmin) +
  geom_point(aes(elevation, in_sf), color = "gray60", alpha = 0.2, size = 3) +
  stat_function(fun = funintuicion, data = df, color = "darkred") +
  
  geom_point(aes(elevation, ypred), color = "blue", alpha = 0.2, size = 3) +
  
  theme_minimal()


lm(y ~ x, data = datos)

modrl <- glm(in_sf ~ elevation, data = dmin, family = binomial)

modrl

funglm <- function(x){
  1/(1+exp(-( -1.63089 + 0.06714*x )))
}
  
ggplot(dmin) +
  geom_point(aes(elevation, in_sf), color = "gray60", alpha = 0.2, size = 3) +
  stat_function(fun = funintuicion, data = df, color = "darkred") +
  stat_function(fun = funglm, data = df, color = "blue") +
  theme_minimal()

dmin <- dmin |> 
  mutate(
    y_pred_int = funintuicion(elevation),
    error_int = in_sf - y_pred_int,
    y_pred_glm = funglm(elevation),
    error_glm = in_sf - y_pred_glm,
  ) 

dmin

# accuracy (achunte, contar)
# rmse (sumar errores promediar)

dmin |>
  summarise(
    rmse_int = sqrt(mean(error_int^2)),
    rmse_glm = sqrt(mean(error_glm^2))
    )


