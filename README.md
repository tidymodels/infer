
-   [Installation](#installation)
    -   [Hypothesis tests](#hypothesis-tests)
        -   [Randomization-based Examples](#randomization-based-examples)
        -   [One numerical variable (mean)](#one-numerical-variable-mean)
        -   [One numerical variable (median)](#one-numerical-variable-median)
        -   [One numerical variable (standard deviation)](#one-numerical-variable-standard-deviation)
        -   [One categorical (2 level) variable](#one-categorical-2-level-variable)
        -   [Two categorical (2 level) variables](#two-categorical-2-level-variables)
        -   [One categorical (&gt;2 level) - GoF](#one-categorical-2-level---gof)
        -   [Two categorical (&gt;2 level) variables](#two-categorical-2-level-variables-1)
        -   [One numerical variable, one categorical (2 levels) (diff in means)](#one-numerical-variable-one-categorical-2-levels-diff-in-means)
        -   [One numerical variable one categorical (2 levels) (diff in medians)](#one-numerical-variable-one-categorical-2-levels-diff-in-medians)
        -   [One numerical one categorical (&gt;2 levels) - ANOVA](#one-numerical-one-categorical-2-levels---anova)
        -   [Two numerical vars - SLR](#two-numerical-vars---slr)
    -   [Confidence intervals](#confidence-intervals)
        -   [One numerical (one mean)](#one-numerical-one-mean)
        -   [One numerical (one median)](#one-numerical-one-median)
        -   [One numerical (standard deviation)](#one-numerical-standard-deviation)
        -   [One categorical (one proportion)](#one-categorical-one-proportion)
        -   [One numerical variable one categorical (2 levels) (diff in means)](#one-numerical-variable-one-categorical-2-levels-diff-in-means-1)
        -   [Two categorical variables (diff in proportions)](#two-categorical-variables-diff-in-proportions)
        -   [Two numerical vars - SLR](#two-numerical-vars---slr-1)

Infer: an R package for tidyverse-friendly statistical inference

------------------------------------------------------------------------

[![Travis-CI Build Status](https://travis-ci.org/andrewpbray/infer.svg?branch=master)](https://travis-ci.org/andrewpbray/infer)

The objective of this package is to perform inference using an expressive statistical grammar that coheres with the `tidyverse` design framework.

Installation
============

To install the current stable version of `infer`, make sure to install `devtools` first:

    install.packages("devtools")
    devtools::install_github("andrewbpray/infer")

To install the development version of `infer` (with `devtools` installed already):

    devtools::install_github("ismayc/infer", ref = "dev")

Hypothesis tests
----------------

![](figs/ht-diagram.png)

#### Randomization-based Examples

These examples assume that `mtcars` has been overwritten so that the variables `cyl`, `vs`, `am`, `gear`, and `carb` are `factor`s.

    mtcars <- as.data.frame(mtcars) %>%
      mutate(cyl = factor(cyl),
             vs = factor(vs),
             am = factor(am),
             gear = factor(gear),
             carb = factor(carb))

Check the package vignettes for further examples using theory-based methods in combination with randomization-based methods.

------------------------------------------------------------------------

### One numerical variable (mean)

    mtcars %>%
      specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
      hypothesize(null = "point", mu = 25) %>% 
      generate(reps = 100, type = "bootstrap") %>% 
      calculate(stat = "mean")

### One numerical variable (median)

    mtcars %>%
      specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
      hypothesize(null = "point", med = 26) %>% 
      generate(reps = 100, type = "bootstrap") %>% 
      calculate(stat = "median")

### One numerical variable (standard deviation)

    mtcars %>%
      specify(response = mpg) %>% # alt: mpg ~ NULL (or mpg ~ 1)
      hypothesize(null = "point", sigma = 5) %>% 
      generate(reps = 100, type = "bootstrap") %>% 
      calculate(stat = "sd")

### One categorical (2 level) variable

    mtcars %>%
      specify(response = am) %>% # alt: am ~ NULL (or am ~ 1)
      hypothesize(null = "point", p = c("1" = .25)) %>% 
      generate(reps = 100, type = "simulate") %>% 
      calculate(stat = "prop")

### Two categorical (2 level) variables

    mtcars %>%
      specify(am ~ vs) %>% # alt: response = am, explanatory = vs
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in props")

### One categorical (&gt;2 level) - GoF

    mtcars %>%
      specify(cyl ~ NULL) %>% # alt: response = cyl
      hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
      generate(reps = 100, type = "simulate") %>%
      calculate(stat = "Chisq")

### Two categorical (&gt;2 level) variables

    mtcars %>%
      specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "Chisq")

### One numerical variable, one categorical (2 levels) (diff in means)

    mtcars %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in means")

### One numerical variable one categorical (2 levels) (diff in medians)

    mtcars %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "diff in medians")

### One numerical one categorical (&gt;2 levels) - ANOVA

    mtcars %>%
      specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "F")

### Two numerical vars - SLR

    mtcars %>%
      specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "permute") %>%
      calculate(stat = "slope")

Confidence intervals
--------------------

### One numerical (one mean)

    mtcars %>%
      specify(response = mpg) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "mean")

### One numerical (one median)

    mtcars %>%
      specify(response = mpg) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "median")

### One numerical (standard deviation)

    mtcars %>%
      specify(response = mpg) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "sd")

### One categorical (one proportion)

    mtcars %>%
      specify(response = am) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "prop", success = "1")

### One numerical variable one categorical (2 levels) (diff in means)

    mtcars %>%
      specify(mpg ~ am) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "diff in means")

### Two categorical variables (diff in proportions)

    mtcars %>%
      specify(am ~ vs) %>%
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "diff in props")

### Two numerical vars - SLR

    mtcars %>%
      specify(mpg ~ hp) %>% 
      generate(reps = 100, type = "bootstrap") %>%
      calculate(stat = "slope")
