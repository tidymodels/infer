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
  specify(origin ~ season) %>% # alt: response = origin, explanatory = season
  calculate(stat = "Chisq")

## ------------------------------------------------------------------------
obs_chisq <- fli_small %>% 
  chisq_test(formula = origin ~ season) %>% 
  dplyr::select(statistic)

## ------------------------------------------------------------------------
obs_chisq <- fli_small %>% 
  chisq_stat(formula = origin ~ season)

## ------------------------------------------------------------------------
chisq_null_perm <- fli_small %>%
  specify(origin ~ season) %>% # alt: response = origin, explanatory = season
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq")

visualize(chisq_null_perm) +
  shade_p_value(obs_stat = obs_chisq, direction = "greater")

## ------------------------------------------------------------------------
chisq_null_perm %>% 
  get_p_value(obs_stat = obs_chisq, direction = "greater")

## ------------------------------------------------------------------------
chisq_null_theor <- fli_small %>%
  specify(origin ~ season) %>% 
  hypothesize(null = "independence") %>%
  # generate() ## Not used for theoretical
  calculate(stat = "Chisq")

visualize(chisq_null_theor, method = "theoretical") +
  shade_p_value(obs_stat = obs_chisq, direction = "right")

## ------------------------------------------------------------------------
visualize(chisq_null_perm, method = "both") +
  shade_p_value(obs_stat = obs_chisq, direction = "right")

## ------------------------------------------------------------------------
fli_small %>% 
  chisq_test(formula = origin ~ season) %>% 
  dplyr::pull(p_value)

