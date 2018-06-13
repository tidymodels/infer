## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5) 

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
null_distn %>% visualize(obs_stat = x_bar)
null_distn %>%
  summarize(p_value = mean(stat >= pull(x_bar)) * 2)

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
null_distn %>% visualize(obs_stat = t_bar)
null_distn %>%
  summarize(p_value = mean(stat >= pull(t_bar)) * 2)

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
null_distn %>% visualize(obs_stat = x_tilde)
null_distn %>%
  summarize(p_value = mean(stat <= pull(x_tilde)) * 2)

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
null_distn %>% visualize(obs_stat = p_hat)
null_distn %>%
  summarize(p_value = mean(stat <= pull(p_hat)) * 2)

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
null_distn %>% visualize(obs_stat = d_hat)
null_distn %>%
  summarize(p_value = mean(stat <= pull(d_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = z_hat)
null_distn %>%
  summarize(p_value = mean(stat <= pull(z_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = Chisq_hat)
null_distn %>%
  summarize(p_value = mean(stat >= pull(Chisq_hat))) %>%
  pull()

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
null_distn %>% visualize(obs_stat = Chisq_hat)
null_distn %>%
  summarize(p_value = mean(stat >= pull(Chisq_hat))) %>%
  pull()

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
null_distn %>% visualize(obs_stat = d_hat)
null_distn %>%
  summarize(p_value = mean(stat >= pull(d_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = t_hat)
null_distn %>%
  summarize(p_value = mean(stat >= pull(t_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = d_hat)
null_distn %>%
  summarize(p_value = mean(stat >= pull(d_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = F_hat)
null_distn %>% 
  summarize(p_value = mean(stat >= pull(F_hat))) %>%
  pull()

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
null_distn %>% visualize(obs_stat = slope_hat)
null_distn %>% 
  summarize(p_value = mean(stat >= pull(slope_hat)) * 2) %>%
  pull()

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
null_distn %>% visualize(obs_stat = correlation_hat)
null_distn %>% 
  summarize(p_value = mean(stat >= pull(correlation_hat)) * 2) %>%
  pull()

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
#  null_distn %>% visualize(obs_stat = t_hat)
#  null_distn %>%
#    summarize(p_value = mean(stat >= pull(t_hat)) * 2) %>%
#    pull()

## ------------------------------------------------------------------------
( x_bar <- fli_small %>% 
  specify(response = arr_delay) %>%
  calculate(stat = "mean") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(response = arr_delay) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "mean")
visualize(boot)
c(lower = pull(x_bar) - 2 * sd(pull(boot)),
  upper = pull(x_bar) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( t_bar <- fli_small %>% 
  specify(response = arr_delay) %>%
  calculate(stat = "t") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(response = arr_delay) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t")
visualize(boot)
c(lower = pull(t_bar) - 2 * sd(pull(boot)),
  upper = pull(t_bar) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( p_hat <- fli_small %>% 
   specify(response = day_hour, success = "morning") %>%
   calculate(stat = "prop") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
 specify(response = day_hour, success = "morning") %>%
 generate(reps = 1000, type = "bootstrap") %>%
 calculate(stat = "prop")
visualize(boot)
c(lower = pull(p_hat) - 2 * sd(pull(boot)),
 upper = pull(p_hat) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( d_hat <- fli_small %>%
  specify(arr_delay ~ season) %>%
  calculate(stat = "diff in means", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ season) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "diff in means", order = c("summer", "winter"))
visualize(boot)
c(lower = pull(d_hat) - 2 * sd(pull(boot)), 
  upper = pull(d_hat) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( t_hat <- fli_small %>%
  specify(arr_delay ~ season) %>%
  calculate(stat = "t", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ season) %>%
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "t", order = c("summer", "winter"))
visualize(boot)
c(lower = pull(t_hat) - 2 * sd(pull(boot)), 
  upper = pull(t_hat) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( d_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "diff in props", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "diff in props", order = c("summer", "winter"))
visualize(boot)
c(lower = pull(d_hat) - 2 * sd(pull(boot)), 
  upper = pull(d_hat) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( z_hat <- fli_small %>% 
  specify(day_hour ~ season, success = "morning") %>%
  calculate(stat = "z", order = c("summer", "winter")) )

## ------------------------------------------------------------------------
boot <- fli_small %>%
  specify(day_hour ~ season, success = "morning") %>%
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "z", order = c("summer", "winter"))
visualize(boot)
c(lower = pull(z_hat) - 2 * sd(pull(boot)), 
  upper = pull(z_hat) + 2 * sd(pull(boot)))

## ------------------------------------------------------------------------
( slope_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>%
  calculate(stat = "slope") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "slope")
visualize(boot)
c(lower = pull(slope_hat) - 2 * sd(pull(boot)), 
  upper = pull(slope_hat) + 2 * sd(pull(boot)))   

## ------------------------------------------------------------------------
( correlation_hat <- fli_small %>% 
  specify(arr_delay ~ dep_delay) %>%
  calculate(stat = "correlation") )

## ------------------------------------------------------------------------
boot <- fli_small %>%
   specify(arr_delay ~ dep_delay) %>% 
   generate(reps = 1000, type = "bootstrap") %>%
   calculate(stat = "correlation")
visualize(boot)
c(lower = pull(correlation_hat) - 2 * sd(pull(boot)), 
  upper = pull(correlation_hat) + 2 * sd(pull(boot)))   

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
#  visualize(boot)
#  c(lower = pull(t_hat) - 2 * sd(pull(boot)),
#    upper = pull(t_hat) + 2 * sd(pull(boot)))

