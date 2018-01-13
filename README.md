
Infer
=====

------------------------------------------------------------------------

[![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/infer)](https://cran.r-project.org/package=infer) [![Travis-CI Build Status](https://travis-ci.org/andrewpbray/infer.svg?branch=master)](https://travis-ci.org/andrewpbray/infer) [![Coverage Status](https://img.shields.io/codecov/c/github/andrewpbray/infer/master.svg)](https://codecov.io/github/andrewpbray/infer/?branch=master)

The objective of this package is to perform statistical inference using an expressive statistical grammar that coheres with the `tidyverse` design framework.

![](https://raw.githubusercontent.com/andrewpbray/infer/master/figs/ht-diagram.png)

### Installation

------------------------------------------------------------------------

To install the current stable version of `infer` from CRAN:

``` r
install.packages("infer")
```

To install the developmental version of `infer`, make sure to install `remotes` first:

``` r
install.packages("remotes")
remotes::install_github("andrewpbray/infer")
```

### Examples

------------------------------------------------------------------------

These examples assume that `mtcars` has been overwritten so that the variables `cyl`, `vs`, `am`, `gear`, and `carb` are `factor`s.

``` r
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
          vs = factor(vs),
          am = factor(am),
          gear = factor(gear),
          carb = factor(carb))
```

Hypothesis test for a difference in proportions:

``` r
mtcars %>%
  specify(am ~ vs, success = "1") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props", order = c("1", "0"))
```

Confidence interval for a difference in means:

``` r
mtcars %>%
  specify(mpg ~ am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))
```
