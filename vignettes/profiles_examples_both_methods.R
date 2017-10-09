## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5) 

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

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(response = age) %>% # alt: age ~ NULL (or age ~ 1)
#    hypothesize(null = "point", mu = 50) %>%
#    generate(reps = 1000, type = "bootstrap") %>%
#    calculate(stat = "t") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(response = sex) %>% # alt: sex ~ NULL (or sex ~ 1)
#    hypothesize(null = "point", p = c("m" = .65)) %>%
#    generate(reps = 1000, type = "simulate") %>%
#    calculate(stat = "z") %>%
#    visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(sex ~ city) %>% # alt: response = sex, explanatory = vs
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "diff in props") %>%
#    visualize()

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
#    specify(drugs ~ status) %>% # alt: response = drugs, explanatory = status
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "Chisq") %>%
#    visualize()

## ------------------------------------------------------------------------
prof_small %>%
  specify(age ~ sex) %>% # alt: response = age, explanatory = sex
  hypothesize(null = "independence") %>%
  generate(reps = 1000, type = "permute") %>%
  calculate(stat = "t") %>% 
  visualize()

## ----eval=FALSE, include=FALSE-------------------------------------------
#  prof_small %>%
#    specify(age ~ status) %>% # alt: response = age, explanatory = status
#    hypothesize(null = "independence") %>%
#    generate(reps = 1000, type = "permute") %>%
#    calculate(stat = "F") %>%
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

