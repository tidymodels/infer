## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 6, fig.height = 3.5) 
options(digits = 4)

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
( x_bar <- fli_small %>%
  specify(response = dep_delay) %>%
  calculate(stat = "mean") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(response = dep_delay) %>%
  hypothesize(null = "point", mu = 10) %>%
  generate(reps = 1000) %>%
  calculate(stat = "mean")

visualize(null_distn) +
  shade_p_value(obs_stat = x_bar, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = x_bar, direction = "two_sided")

## ------------------------------------------------------------------------
( t_bar <- fli_small %>%
  specify(response = dep_delay) %>%
  calculate(stat = "t") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(response = dep_delay) %>%
  hypothesize(null = "point", mu = 8) %>%
  generate(reps = 1000) %>%
  calculate(stat = "t")

visualize(null_distn) +
  shade_p_value(obs_stat = t_bar, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = t_bar, direction = "two_sided")

## ------------------------------------------------------------------------
( x_tilde <- fli_small %>%
  specify(response = dep_delay) %>%
  calculate(stat = "median") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(response = dep_delay) %>%
  hypothesize(null = "point", med = -1) %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "median")

visualize(null_distn) +
  shade_p_value(obs_stat = x_tilde, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = x_tilde, direction = "two_sided")

## ------------------------------------------------------------------------
( p_hat <- fli_small %>%
  specify(response = day_hour, success = "morning") %>%
  calculate(stat = "prop") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(response = day_hour, success = "morning") %>%
  hypothesize(null = "point", p = .5) %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")

visualize(null_distn) +
  shade_p_value(obs_stat = p_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = p_hat, direction = "two_sided")

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  mutate(day_hour_logical = (day_hour == "morning")) %>%
  specify(response = day_hour_logical, success = "TRUE") %>%
  hypothesize(null = "point", p = .5) %>%
  generate(reps = 1000) %>%
  calculate(stat = "prop")

## ------------------------------------------------------------------------
( d_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "diff in props", order = c("winter", "summer")) )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "diff in props", order = c("winter", "summer"))

visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( z_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "z", order = c("winter", "summer")) )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000) %>% 
  calculate(stat = "z", order = c("winter", "summer"))

visualize(null_distn) +
  shade_p_value(obs_stat = z_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = z_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( Chisq_hat <- fli_small %>%
  specify(response = origin) %>%
  hypothesize(null = "point", 
              p = c("EWR" = .33, "JFK" = .33, "LGA" = .34)) %>% 
  calculate(stat = "Chisq") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(response = origin) %>%
  hypothesize(null = "point", 
              p = c("EWR" = .33, "JFK" = .33, "LGA" = .34)) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  calculate(stat = "Chisq")

visualize(null_distn) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
null_distn %>%
  get_p_value(obs_stat = Chisq_hat, direction = "greater")

## ------------------------------------------------------------------------
( Chisq_hat <- fli_small %>%
  specify(formula = day_hour ~ origin) %>% 
  calculate(stat = "Chisq") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(day_hour ~ origin) %>%
  hypothesize(null = "independence") %>% 
  generate(reps = 1000, type = "permute") %>% 
  calculate(stat = "Chisq")

visualize(null_distn) +
  shade_p_value(obs_stat = Chisq_hat, direction = "greater")
null_distn %>%
  get_p_value(obs_stat = Chisq_hat, direction = "greater")

## ------------------------------------------------------------------------
( d_hat <- fli_small %>% 
  specify(dep_delay ~ season) %>% 
  calculate(stat = "diff in means", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(dep_delay ~ season) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("summer", "winter"))

visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( t_hat <- fli_small %>% 
  specify(dep_delay ~ season) %>% 
  calculate(stat = "t", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(dep_delay ~ season) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("summer", "winter"))

visualize(null_distn) +
  shade_p_value(obs_stat = t_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = t_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( d_hat <- fli_small %>% 
  specify(dep_delay ~ season) %>% 
  calculate(stat = "diff in medians", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
  specify(dep_delay ~ season) %>% # alt: response = dep_delay, 
  # explanatory = season
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in medians", order = c("summer", "winter"))

visualize(null_distn) +
  shade_p_value(obs_stat = d_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = d_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( F_hat <- fli_small %>% 
  specify(arr_delay ~ origin) %>%
  calculate(stat = "F") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
   specify(arr_delay ~ origin) %>%
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "F")

visualize(null_distn) +
  shade_p_value(obs_stat = F_hat, direction = "greater")
null_distn %>%
  get_p_value(obs_stat = F_hat, direction = "greater")

## ------------------------------------------------------------------------
( slope_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>% 
  calculate(stat = "slope") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "slope")

visualize(null_distn) +
  shade_p_value(obs_stat = slope_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = slope_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( correlation_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>% 
  calculate(stat = "correlation") )

## ------------------------------------------------------------------------
null_distn <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   hypothesize(null = "independence") %>%
   generate(reps = 1000, type = "permute") %>%
   calculate(stat = "correlation")

visualize(null_distn) +
  shade_p_value(obs_stat = correlation_hat, direction = "two_sided")
null_distn %>%
  get_p_value(obs_stat = correlation_hat, direction = "two_sided")

## ----echo=FALSE, eval=FALSE----------------------------------------------
#  # **Standardized observed stat**
#  ( t_hat <- fli_small %>%
#    specify(arr_delay ~ dep_delay) %>%
#    calculate(stat = "t") )

## ----echo=FALSE, eval=FALSE----------------------------------------------
#  null_distn <- fli_small %>%
#     specify(arr_delay ~ dep_delay) %>%
#     hypothesize(null = "independence") %>%
#     generate(reps = 1000, type = "permute") %>%
#     calculate(stat = "t")
#  
#  visualize(null_distn) +
#    shade_p_value(obs_stat = t_hat, direction = "two_sided")
#  null_distn %>%
#    get_p_value(obs_stat = t_hat, direction = "two_sided")

## ------------------------------------------------------------------------
( x_bar <- fli_small %>% 
  specify(response = arr_delay) %>%
  calculate(stat = "mean") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(response = arr_delay) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "mean")
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = x_bar) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( t_hat <- fli_small %>% 
  specify(response = arr_delay) %>%
  calculate(stat = "t") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(response = arr_delay) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t")
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = t_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( p_hat <- fli_small %>% 
   specify(response = day_hour, success = "morning") %>%
   calculate(stat = "prop") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
 specify(response = day_hour, success = "morning") %>%
 generate(reps = 1000, type = "bootstrap") %>%
 calculate(stat = "prop")
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = p_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( d_hat <- fli_small %>%
  specify(arr_delay ~ season) %>%
  calculate(stat = "diff in means", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ season) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "diff in means", order = c("summer", "winter"))
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = d_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( t_hat <- fli_small %>%
  specify(arr_delay ~ season) %>%
  calculate(stat = "t", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ season) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t", order = c("summer", "winter"))
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = t_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( d_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "diff in props", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("summer", "winter"))
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = d_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( z_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "z", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "z", order = c("summer", "winter"))
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = z_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( slope_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>%
  calculate(stat = "slope") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "slope")
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", point_estimate = slope_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ------------------------------------------------------------------------
( correlation_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>%
  calculate(stat = "correlation") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "correlation")
( percentile_ci <- get_ci(boot) )

visualize(boot) +
  shade_confidence_interval(endpoints = percentile_ci)
( standard_error_ci <- get_ci(boot, type = "se", 
                            point_estimate = correlation_hat) )

visualize(boot) +
  shade_confidence_interval(endpoints = standard_error_ci)

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  # **Point estimate**
#  ( t_hat <- fli_small %>%
#    specify(arr_delay ~ dep_delay) %>%
#    calculate(stat = "t") )

## ----eval=FALSE, echo=FALSE----------------------------------------------
#  boot <- fli_small %>%
#     specify(arr_delay ~ dep_delay) %>%
#     generate(reps = 1000, type = "bootstrap") %>%
#     calculate(stat = "t")
#  ( percentile_ci <- get_ci(boot) )
#  
#  visualize(boot) +
#    shade_confidence_interval(endpoints = percentile_ci)
#  ( standard_error_ci <- get_ci(boot, type = "se", point_estimate = t_hat) )
#  
#  visualize(boot) +
#    shade_confidence_interval(endpoints = standard_error_ci)

