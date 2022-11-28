library(tidyverse)
library(partykit)

# predecir el valor de la variable estado.

# d: datos - entr: entrenamiento
dentr <- read_csv("https://raw.githubusercontent.com/jbkunst/intro-machine-learning/main/data/datos_credito_train.csv")

# es lo primero de debemos hacer.
glimpse(dentr)

# necesario para R
# convertir todo lo que es caracter a factor
# caracter/string: vector de caracteres.
# factor: vector con categorias
dentr <- mutate_if(dentr, is.character, as.factor)
dentr |> count(estado)

# un modelo con TODAS las variables ---------------------------------------
mod_rl <- glm(estado ~ ., data = dentr, family = binomial)

# stepwise: como le damos el modelo full, es un stepwise "backward"
mod_rl <- step(mod_rl)

summary(mod_rl)


# un arbol con todas las variables ----------------------------------------
mod_ad <- ctree(estado ~ ., data = dentr)

plot(mod_ad)
mod_ad

# aplicarlo a la muestra de validacion ------------------------------------
# leer datos
dval <- read_csv("https://raw.githubusercontent.com/jbkunst/intro-machine-learning/main/data/datos_credito_test.csv")
dval <- mutate_if(dval, is.character, as.factor)


predict(mod_ad, newdata = dval, type = "response")
predict(mod_ad, newdata = dval, type = "prob")[, 2]

# d: data, val: validacion, min: minizada
dvalmin <- dval |> 
  mutate(
    pred_rl = predict(mod_rl, newdata = dval, type = "response"),
    pred_ad = predict(mod_ad, newdata = dval),
    # .before es para posicionar la columnas creadas
    .before = 1
  ) |> 
  select(pred_rl, pred_ad, estado)

# accuracy del arbol:
# contamos los casos donde la predicción es igual al valor a predecir:
dvalmin |> 
  mutate(achunte = pred_ad == estado) |> 
  count(achunte) |> 
  mutate(p = n/sum(n))
  
# accuray de la rl, 
# dictomizar la probabilidad del modelo
# contamos los casos donde la predicción es igual al valor a predecir:
dvalmin |> 
  mutate(
    pred_rl_bin = ifelse(pred_rl > 0.5, "malo", "bueno"),
    achunte     = pred_rl_bin == estado
    ) |> 
  count(achunte) |> 
  mutate(p = n/sum(n))

message("Tree wins!")

