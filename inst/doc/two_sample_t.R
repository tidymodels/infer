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
  specify(arr_delay ~ half_year) %>%
  calculate(stat = "t", order = c("h1", "h2"))

## ------------------------------------------------------------------------
obs_t <- fli_small %>% 
  t_test(formula = arr_delay ~ half_year, alternative = "two_sided",
         order = c("h1", "h2")) %>% 
  dplyr::select(statistic) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
obs_t <- fli_small %>% 
  t_stat(formula = arr_delay ~ half_year, order = c("h1", "h2"))

## ------------------------------------------------------------------------
t_null_distn <- fli_small %>%
  # alt: response = arr_delay, explanatory = half_year
  specify(arr_delay ~ half_year) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("h1", "h2"))
t_null_distn %>% visualize(obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
t_null_distn %>% 
  p_value(obs_stat = obs_t, direction = "greater")

## ------------------------------------------------------------------------
fli_small %>%
  # alt: response = arr_delay, explanatory = half_year
  specify(arr_delay ~ half_year) %>%
  hypothesize(null = "independence") %>%
  # generate() ## Not used for theoretical
  calculate(stat = "t", order = c("h1", "h2")) %>%
  visualize(method = "theoretical", obs_stat = obs_t, direction = "two_sided")

## ----eval=FALSE----------------------------------------------------------
#  fli_small %>%
#    # alt: response = arr_delay, explanatory = half_year
#    specify(arr_delay ~ half_year) %>%
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "t", order = c("h1", "h2")) %>%
#    visualize(method = "both", obs_stat = obs_t, direction = "two_sided")

## ----echo=FALSE----------------------------------------------------------
# To use same distribution calculated above
t_null_distn %>% 
  visualize(method = "both", obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
fli_small %>% 
  t_test(formula = arr_delay ~ half_year,
         alternative = "two_sided",
         order = c("h1", "h2")) %>% 
  dplyr::select(p_value) %>% 
  dplyr::pull()

