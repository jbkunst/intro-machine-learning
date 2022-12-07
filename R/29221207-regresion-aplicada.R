library(tidyverse)
library(lubridate)
library(equatiomatic)
library(latex2exp)

datos <- read_csv("data/bike_day.csv")

datos <- datos |>
  select(-casual, -registered)

glimpse(datos)

ggplot(datos) +
    geom_point(aes(dteday, cnt))

datos <- datos |> 
  mutate(muestra = ifelse(dteday < ymd(20120701), "train", "test"))

ggplot(datos) +
  geom_point(aes(dteday, cnt, color = muestra))


fit_bike <- function(fmla = cnt ~ yr, fitfun = lm) {
  
  dentrenaimento <-  datos |> 
    filter(muestra == "train") |> 
    select(-muestra)
  
  mod <- fitfun(formula = fmla, data = dentrenaimento)
  
  cap <- ""
  if(any(class(mod) %in% "lm")){
    # extract equation with `ital_vars = TRUE` to avoid the use of `\operatorname`
    m_eq <- extract_eq(mod, use_coef = TRUE, ital_vars = TRUE)
    # swap escaped underscores for dashes
    prep_eq <- gsub("\\\\_", "-", m_eq)
    # swap display-style $$ with inline-style $
    prep_eq <- paste("$", as.character(prep_eq), "$", sep = "")  
    cap <- prep_eq 
  }
  
  dplot <- datos |> 
    mutate(pred = predict(mod, newdata = datos)) |> 
    select(dteday, cnt, pred, muestra)
  
  ggplot(dplot) +
    geom_point(aes(dteday, cnt, color = muestra)) +
    geom_line(aes(dteday, pred), color = "purple") +
    scale_color_viridis_d(begin = 0.1, end = 0.9) +
    scale_y_continuous(labels = scales::comma) +
    labs(color = "", caption = latex2exp::TeX(cap)) +
    theme_minimal() +
    theme(legend.position =  "bottom")
  
}


fit_bike(cnt ~ yr)

fit_bike(cnt ~ yr + mnth + season)

datos |> 
  select(casual, registered,   cnt) |> 
  mutate(casual + registered)



           