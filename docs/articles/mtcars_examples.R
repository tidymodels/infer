## ----include=FALSE-------------------------------------------------------
knitr::opts_chunk$set(fig.width = 8, fig.height = 5) 

## ----message=FALSE, warning=FALSE----------------------------------------
library(nycflights13)
library(dplyr)
library(ggplot2)
library(stringr)
library(infer)
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

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

