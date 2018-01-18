## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(infer)
library(dplyr)
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))
# For reproducibility         
set.seed(2018)         

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", mu = 25) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "mean")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", med = 26) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "median")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", sigma = 5) %>% 
  generate(reps = 100, type = "bootstrap") %>% 
  calculate(stat = "sd")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = am, success = "1") %>% # formula alt: am ~ NULL
  hypothesize(null = "point", p = .25) %>% 
  generate(reps = 100, type = "simulate") %>% 
  calculate(stat = "prop")

## ------------------------------------------------------------------------
mtcars %>%
  specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props", order = c("0", "1"))

## ------------------------------------------------------------------------
mtcars %>%
  specify(cyl ~ NULL) %>% # alt: response = cyl
  hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
  generate(reps = 100, type = "simulate") %>%
  calculate(stat = "Chisq")

## ----warning = FALSE-----------------------------------------------------
mtcars %>%
  specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "Chisq")

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in means", order = c("0", "1"))

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in medians", order = c("0", "1"))

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "F")

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "slope")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "mean")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "median")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = mpg) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "sd")

## ------------------------------------------------------------------------
mtcars %>%
  specify(response = am, success = "1") %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "prop")

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("0", "1"))

## ------------------------------------------------------------------------
mtcars %>%
  specify(am ~ vs, success = "1") %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in props", order = c("0", "1"))

## ------------------------------------------------------------------------
mtcars %>%
  specify(mpg ~ hp) %>% 
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "slope")

