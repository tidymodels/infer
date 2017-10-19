## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 3) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(nycflights13)
library(dplyr)
library(stringr)
library(infer)
set.seed(2017)
fli_small <- flights %>% 
  sample_n(size = 500) %>% 
  mutate(half_year = case_when(
    between(month, 1, 6) ~ "h1",
    between(month, 7, 12) ~ "h2"
  )) %>% 
  mutate(day_hour = case_when(
    between(hour, 1, 12) ~ "morning",
    between(hour, 13, 24) ~ "not morning"
  )) %>% 
  select(arr_delay, dep_delay, half_year, 
         day_hour, origin, carrier)

## ------------------------------------------------------------------------
obs_t <- fli_small %>% 
  t_test(formula = arr_delay ~ half_year) %>% 
  dplyr::select(statistic) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
obs_t <- fli_small %>% 
  t_stat(formula = arr_delay ~ half_year)

## ------------------------------------------------------------------------
t_null_distn <- fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t")
t_null_distn %>% visualize(obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
t_null_distn %>% 
  dplyr::summarize(p_value = mean(abs(stat) >= obs_t)) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  # calculate(stat = "t") ## Not needed since t is implied based on variable types
  visualize(method = "theoretical", obs_stat = obs_t, direction = "two_sided")

## ----eval=FALSE----------------------------------------------------------
#  fli_small %>%
#    specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "t") %>%
#    visualize(method = "both", obs_stat = obs_t, direction = "two_sided")

## ----echo=FALSE----------------------------------------------------------
# To use same distribution calculated above
t_null_distn %>% 
  visualize(method = "both", obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
fli_small %>% 
  t_test(formula = arr_delay ~ half_year) %>% 
  dplyr::select(p_value) %>% 
  dplyr::pull()

