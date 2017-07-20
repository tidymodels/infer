
Timing analysis
===============

This analysis is for 10000 flights with `reps =` `1000`.

``` r
devtools::session_info()
```

    ## Session info -------------------------------------------------------------

    ##  setting  value                       
    ##  version  R version 3.4.1 (2017-06-30)
    ##  system   x86_64, darwin15.6.0        
    ##  ui       X11                         
    ##  language (EN)                        
    ##  collate  en_US.UTF-8                 
    ##  tz       America/Los_Angeles         
    ##  date     2017-07-20

    ## Packages -----------------------------------------------------------------

    ##  package   * version date       source        
    ##  backports   1.1.0   2017-05-22 CRAN (R 3.4.0)
    ##  base      * 3.4.1   2017-07-07 local         
    ##  compiler    3.4.1   2017-07-07 local         
    ##  datasets  * 3.4.1   2017-07-07 local         
    ##  devtools    1.13.2  2017-06-02 CRAN (R 3.4.0)
    ##  digest      0.6.12  2017-01-27 CRAN (R 3.4.0)
    ##  evaluate    0.10.1  2017-06-24 CRAN (R 3.4.0)
    ##  graphics  * 3.4.1   2017-07-07 local         
    ##  grDevices * 3.4.1   2017-07-07 local         
    ##  htmltools   0.3.6   2017-04-28 CRAN (R 3.4.0)
    ##  knitr       1.16    2017-05-18 CRAN (R 3.4.0)
    ##  magrittr    1.5     2014-11-22 CRAN (R 3.4.0)
    ##  memoise     1.1.0   2017-04-21 CRAN (R 3.4.0)
    ##  methods   * 3.4.1   2017-07-07 local         
    ##  Rcpp        0.12.11 2017-05-22 CRAN (R 3.4.0)
    ##  rmarkdown   1.6     2017-06-15 CRAN (R 3.4.0)
    ##  rprojroot   1.2     2017-01-16 CRAN (R 3.4.0)
    ##  stats     * 3.4.1   2017-07-07 local         
    ##  stringi     1.1.5   2017-04-07 CRAN (R 3.4.0)
    ##  stringr     1.2.0   2017-02-18 CRAN (R 3.4.0)
    ##  tools       3.4.1   2017-07-07 local         
    ##  utils     * 3.4.1   2017-07-07 local         
    ##  withr       1.0.2   2016-06-20 CRAN (R 3.4.0)
    ##  yaml        2.1.14  2016-11-12 CRAN (R 3.4.0)

``` r
library(nycflights13)
library(tidyverse)
library(stringr)
library(infer)
set.seed(2017)
fli_small <- flights %>% 
  na.omit() %>% 
  sample_n(size = params$num_rows) %>% 
  mutate(half_year = case_when(
    between(month, 1, 6) ~ "h1",
    between(month, 7, 12) ~ "h2"
  )) %>% 
  mutate(day_hour = case_when(
    between(hour, 1, 12) ~ "morning",
    between(hour, 13, 24) ~ "not morning"
  )) %>% 
  select(arr_delay, dep_delay, half_year, 
         day_hour, origin, carrier)
```

-   Two numeric - `arr_delay`, `dep_delay`
-   Two categories
    -   `half_year` (`"h1"`, `"h2"`),
    -   `day_hour` (`"morning"`, `"not morning"`)
-   Three categories - `origin` (`"EWR"`, `"JFK"`, `"LGA"`)
-   Sixteen categories - `carrier`

------------------------------------------------------------------------

``` r
library(rbenchmark)
```

One numerical variable (mean)

``` r
benchmark(
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", mu = 50) %>% 
  generate(reps = params$rep_times, type = "bootstrap") %>% 
  calculate(stat = "mean"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   1.585

One numerical variable (median)

``` r
benchmark(
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", Med = 55) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "median"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1    1.47

One categorical (2 level) variable

``` r
benchmark(
fli_small %>%
  specify(response = half_year) %>% # alt: half_year ~ NULL (or half_year ~ 1)
  hypothesize(null = "point", p = c("h1" = .45)) %>% 
  generate(reps = params$rep_times, type = "simulate") %>% 
  calculate(stat = "prop"),
replications = 1, columns = "elapsed"
)
```

    ## Warning in parse_params(dots, x): Missing level, assuming proportion is 1 -
    ## 0.45.

    ## Warning in parse_params(dots, x): Missing level, assuming proportion is 1 -
    ## 0.45.

    ##   elapsed
    ## 1   10.63

Two categorical (2 level) variables

``` r
benchmark(
fli_small %>%
  specify(half_year ~ day_hour) %>% # alt: response = half_year, explanatory = vs
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in props"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   6.226

One categorical (&gt;2 level) - GoF

``` r
benchmark(
fli_small %>%
  specify(origin ~ NULL) %>% # alt: response = day_hour
  hypothesize(null = "point", 
              p = c("EWR" = .3, "JFK" = .4, "LGA" = .3)) %>%
  generate(reps = params$rep_times, type = "simulate") %>%
  calculate(stat = "Chisq"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1    7.79

Two categorical (&gt;2 level) variables

``` r
benchmark(
fli_small %>%
  specify(origin ~ carrier) %>% # alt: response = origin, explanatory = carrier
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "Chisq"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   6.133

One numerical variable one categorical (2 levels) (diff in means)

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in means"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   5.687

One numerical variable one categorical (2 levels) (diff in medians)

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in medians"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   6.197

One numerical one categorical (&gt;2 levels) - ANOVA

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ origin) %>% # alt: response = arr_delay, explanatory = day_hour
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "F"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1  17.624

Two numerical vars - SLR

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ dep_delay) %>% # alt: response = arr_delay, explanatory = dep_delay
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "slope"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1  17.406

### Confidence intervals

One numerical (one mean)

``` r
benchmark(
fli_small %>%
  specify(response = arr_delay) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "mean"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   1.241

One numerical (one median)

``` r
benchmark(
fli_small %>%
  specify(response = arr_delay) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "median"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   1.649

One categorical (one proportion)

``` r
benchmark(
fli_small %>%
  specify(response = half_year) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "prop", success = "h2"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1    5.01

One numerical variable one categorical (2 levels) (diff in means)

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ half_year) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "diff in means"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   1.541

Two categorical variables (diff in proportions)

``` r
benchmark(
fli_small %>%
  specify(half_year ~ day_hour) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "diff in props"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1   2.023

Two numerical vars - SLR

``` r
benchmark(
fli_small %>%
  specify(arr_delay ~ dep_delay) %>% 
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "slope"),
replications = 1, columns = "elapsed"
)
```

    ##   elapsed
    ## 1  11.295
