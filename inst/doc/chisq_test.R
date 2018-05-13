## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(nycflights13)
library(dplyr)
library(ggplot2)
library(stringr)
library(infer)
set.seed(2017)
fli_small <- flights %>% 
  na.omit() %>% 
  sample_n(size = 500) %>% 
  mutate(season = case_when(
    month %in% c(10:12, 1:3) ~ "winter",
    month %in% c(4:9) ~ "summer"
  )) %>% 
  mutate(day_hour = case_when(
    between(hour, 1, 12) ~ "morning",
    between(hour, 13, 24) ~ "not morning"
  )) %>% 
  select(arr_delay, dep_delay, season, 
         day_hour, origin, carrier)

## ------------------------------------------------------------------------
obs_chisq <- fli_small %>% 
  chisq_test(formula = origin ~ season) %>% 
  dplyr::select(statistic) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
obs_chisq <- fli_small %>% 
  chisq_stat(formula = origin ~ season)

## ------------------------------------------------------------------------
chisq_null_distn <- fli_small %>%
  specify(origin ~ season) %>% # alt: response = origin, explanatory = season
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq")
chisq_null_distn %>% visualize(obs_stat = obs_chisq, direction = "greater")

## ------------------------------------------------------------------------
chisq_null_distn %>% 
  dplyr::summarize(p_value = mean(stat >= pull(obs_chisq))) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
fli_small %>%
  specify(origin ~ season) %>% 
  hypothesize(null = "independence") %>%
  # generate() ## Not used for theoretical
  calculate(stat = "Chisq") %>%
  visualize(method = "theoretical", obs_stat = obs_chisq, direction = "right")

## ----eval=FALSE----------------------------------------------------------
#  fli_small %>%
#    specify(origin ~ season) %>%  %>% # alt: response = origin, explanatory = season
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "Chisq") %>%
#    visualize(method = "both", obs_stat = obs_chisq, direction = "right")

## ----echo=FALSE----------------------------------------------------------
# To use same distribution calculated above
chisq_null_distn %>% 
  visualize(method = "both", obs_stat = obs_chisq, direction = "right")

## ------------------------------------------------------------------------
fli_small %>% 
  chisq_test(formula = origin ~ season) %>% 
  dplyr::select(p_value) %>% 
  dplyr::pull()

