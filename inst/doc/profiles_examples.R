## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(okcupiddata)
library(infer)

set.seed(2017)
prof_small <- profiles %>% 
  na.omit() %>% 
  dplyr::sample_n(size = 500) %>% 
  dplyr::mutate(city = dplyr::case_when(
    stringr::str_detect(location, "san fran") ~ "san fran",
    !stringr::str_detect(location, "san fran") ~ "not san fran"
  )) %>% 
  dplyr::select(age, sex, city, drugs, height, status)

## ------------------------------------------------------------------------
x_bar <- prof_small %>% 
  dplyr::summarize(mean(age)) %>% 
  dplyr::pull()
prof_small %>%
  specify(response = age) %>% # alt: age ~ NULL (or age ~ 1)
  hypothesize(null = "point", mu = 34) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "mean") %>% 
  visualize(obs_stat = x_bar, direction = "both")

## ------------------------------------------------------------------------
x_tilde <- prof_small %>% 
  dplyr::summarize(median(age)) %>% 
  dplyr::pull()
prof_small %>%
  specify(response = age) %>% # alt: age ~ NULL
  hypothesize(null = "point", med = 29) %>% 
  generate(reps = 1000, type = "bootstrap") %>% 
  calculate(stat = "median") %>% 
  visualize(obs_stat = x_tilde, direction = "both")

## ------------------------------------------------------------------------
p_hat <- prof_small %>% 
  dplyr::summarize(mean(sex == "f")) %>% 
  dplyr::pull()
prof_small %>%
  specify(response = sex, success = "f") %>%
  hypothesize(null = "point", p = c("f" = .35)) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  calculate(stat = "prop") %>% 
  visualize(obs_stat = p_hat, direction = "right")

## ------------------------------------------------------------------------
obs_diff <- prof_small %>% 
  dplyr::group_by(city) %>% 
  dplyr::summarize(prop = mean(sex == "m")) %>%
  dplyr::summarize(diff(prop)) %>%
  dplyr::pull()
prof_small %>%
  specify(sex ~ city, success = "m") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in props", order = c("san fran", "not san fran")) %>% 
  visualize(obs_stat = obs_diff, direction = "both")

## ------------------------------------------------------------------------
Chisq_hat <- chisq.test(table(prof_small$drugs))$stat
prof_small %>%
  specify(drugs ~ NULL) %>% # alt: response = drugs
  hypothesize(null = "point", 
              p = c("never" = .7, "sometimes" = .25, "often" = .05)) %>%
  generate(reps = 1000, type = "simulate") %>%
  calculate(stat = "Chisq") %>% 
  # Only "right" or "greater" as options here
  visualize(obs_stat = Chisq_hat, direction = "right")

## ------------------------------------------------------------------------
Chisq_hat <- prof_small %>% chisq_stat(formula = drugs ~ status)
prof_small %>%
  specify(drugs ~ status) %>% # alt: response = drugs, explanatory = status
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq") %>% 
  # Only "right" or "greater" as options here
  visualize(obs_stat = Chisq_hat, direction = "right")

## ------------------------------------------------------------------------
obs_diff <- prof_small %>% 
  dplyr::group_by(sex) %>% 
  dplyr::summarize(mean_age = mean(age)) %>% 
  dplyr::summarize(diff(mean_age)) %>% 
  dplyr::pull()
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in means", order = c("m", "f")) %>% 
  visualize(obs_stat = obs_diff, direction = "both")

## ------------------------------------------------------------------------
obs_diff <- prof_small %>% 
  dplyr::group_by(sex) %>% 
  dplyr::summarize(median_age = median(age)) %>% 
  dplyr::summarize(diff(median_age)) %>% 
  dplyr::pull()
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "diff in medians", order = c("m", "f")) %>% 
  visualize(obs_stat = obs_diff, direction = "two_sided")

## ------------------------------------------------------------------------
F_hat <- anova(
               aov(formula = age ~ status, data = prof_small)
               )$`F value`[1]
prof_small %>%
  specify(age ~ status) %>% # alt: response = age, explanatory = status
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F") %>% 
  # Only "right" or "greater" as options here
  visualize(obs_stat = F_hat, direction = "greater")

## ------------------------------------------------------------------------
slope_hat <- lm(age ~ height, data = prof_small) %>% 
  broom::tidy() %>% 
  dplyr::filter(term == "height") %>% 
  dplyr::select(estimate) %>% 
  dplyr::pull()
prof_small %>%
  specify(age ~ height) %>% # alt: response = age, explanatory = height
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "slope") %>% 
  visualize(obs_stat = slope_hat, direction = "both")

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = age) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "mean") %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = age) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "median") %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = sex, success = "f") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "prop") %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("m", "f")) %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(sex ~ city, success = "m") %>%
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("san fran", "not san fran")) %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ height) %>% 
  generate(reps = 1000, type = "bootstrap") %>%
  calculate(stat = "slope") %>% 
  visualize()

