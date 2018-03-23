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
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("m", "f")) %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  # generate() is not needed since we are not doing randomization
  # calculate(stat = "t") ## Not needed since t implied based on variable types
  visualize(method = "theoretical")

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t", order = c("m", "f")) %>% 
  visualize(method = "both")

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ status) %>% # alt: response = age, explanatory = status
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F") %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ status) %>% # alt: response = age, explanatory = status
  # generate() is not needed since we are not doing randomization
  # calculate(stat = "F") ## Not needed since F implied based on variable types
  hypothesize(null = "independence") %>%
  visualize(method = "theoretical")

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ status) %>% # alt: response = age, explanatory = status
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "F") %>% 
  visualize(method = "both")

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = sex, success = "m") %>% 
  hypothesize(null = "point", p = c("m" = .65, "f" = .35)) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  calculate(stat = "z") %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = sex, success = "m") %>%
  hypothesize(null = "point", p = c("m" = .65, "f" = .35)) %>% 
  # generate() is not needed since we are not doing randomization
  # calculate(stat = "z") ## Not needed since z implied based on variable types
  visualize(method = "theoretical")

## ------------------------------------------------------------------------
prof_small %>%
  specify(response = sex, success = "m") %>%
  hypothesize(null = "point", p = c("m" = .65, "f" = .35)) %>% 
  generate(reps = 1000, type = "simulate") %>% 
  calculate(stat = "z") %>% 
  visualize(method = "both")

## ------------------------------------------------------------------------
prof_small %>%
  specify(sex ~ city, success = "m") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "z", order = c("san fran", "not san fran")) %>% 
  visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(sex ~ city, success = "m") %>%
  hypothesize(null = "independence") %>% 
  # generate() is not needed since we are not doing randomization
  # calculate(stat = "z") ## Not needed since z implied based on variable types
  visualize(method = "theoretical")

## ------------------------------------------------------------------------
prof_small %>%
  specify(sex ~ city, success = "m") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "z", order = c("san fran", "not san fran")) %>% 
  visualize(method = "both")

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(drugs ~ status) %>% # alt: response = drugs, explanatory = status
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "Chisq") %>%
#    visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(drugs ~ status) %>% # alt: response = drugs, explanatory = status
  hypothesize(null = "independence") %>%
  # generate() is not needed since we are not doing randomization
  # calculate(stat = "Chisq") ## Not needed since Chisq implied based on variable types
  visualize(method = "theoretical")

## ------------------------------------------------------------------------
prof_small %>%
  specify(drugs ~ status) %>% # alt: response = drugs, explanatory = status
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "Chisq") %>% 
  visualize(method = "both")

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(drugs ~ NULL) %>% # alt: response = drugs
#    hypothesize(null = "point",
#                p = c("never" = .7, "sometimes" = .25, "often" = .05)) %>%
#    generate(reps = 1000, type = "simulate") %>%
#    calculate(stat = "Chisq") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(age ~ height) %>% # alt: response = age, explanatory = height
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "slope") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(response = age) %>% # alt: age ~ NULL (or age ~ 1)
#    hypothesize(null = "point", mu = 50) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "t") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(response = age) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "mean") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(response = sex) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "prop", success = "f") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(age ~ sex) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "t") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(sex ~ city) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "diff in props") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(age ~ height) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "slope") %>%
#    visualize()

