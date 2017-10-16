## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 7, fig.height = 3) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(okcupiddata)
library(stringr)
library(infer)
set.seed(2017)
prof_small <- profiles %>% 
  dplyr::sample_n(size = 500) %>% 
  dplyr::mutate(city = dplyr::case_when(
    str_detect(location, "san fran") ~ "san fran",
    !str_detect(location, "san fran") ~ "not san fran"
  )) %>% 
  dplyr::select(age, sex, city, drugs, height, status)

## ------------------------------------------------------------------------
obs_t <- prof_small %>% 
  t_test(formula = age ~ sex) %>% 
  dplyr::select(statistic) %>% 
  dplyr::pull()

## ------------------------------------------------------------------------
obs_t <- prof_small %>% 
  t_stat(formula = age ~ sex)

## ------------------------------------------------------------------------
t_null_distn <- prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t")
t_null_distn %>% visualize(obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
t_null_distn %>% 
  dplyr::summarize(p_value = mean(abs(stat) >= obs_t))

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  # calculate(stat = "t") ## Not needed since t is implied based on variable types
  visualize(method = "theoretical", obs_stat = obs_t, direction = "two_sided")

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t") %>% 
  visualize(method = "both", obs_stat = obs_t, direction = "two_sided")

