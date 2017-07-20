
These examples use 200 flights from `nycflights13::flights` and 5000 replicates.

``` r
library(nycflights13)
library(tidyverse)
library(stringr)
library(infer)
set.seed(2017)
fli_small <- flights %>% 
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

One numerical variable (mean)

``` r
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", mu = 50) %>% 
  generate(reps = params$rep_times, type = "bootstrap") %>% 
  calculate(stat = "mean", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate     stat
    ##        <int>    <dbl>
    ##  1         1 50.59023
    ##  2         2 53.56099
    ##  3         3 52.81026
    ##  4         4 49.60776
    ##  5         5 48.84880
    ##  6         6 50.67989
    ##  7         7 47.18644
    ##  8         8 51.01514
    ##  9         9 51.50696
    ## 10        10 53.04615
    ## # ... with 4,990 more rows

One numerical variable (median)

``` r
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", Med = 55) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "median", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1  54.0
    ##  2         2  54.0
    ##  3         3  54.0
    ##  4         4  55.0
    ##  5         5  56.5
    ##  6         6  55.0
    ##  7         7  55.0
    ##  8         8  55.5
    ##  9         9  57.0
    ## 10        10  57.0
    ## # ... with 4,990 more rows

One numerical variable (standard deviation)

*Without removing NA*

``` r
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", sd = 40) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "sd")
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1    NA
    ##  2         2    NA
    ##  3         3    NA
    ##  4         4    NA
    ##  5         5    NA
    ##  6         6    NA
    ##  7         7    NA
    ##  8         8    NA
    ##  9         9    NA
    ## 10        10    NA
    ## # ... with 4,990 more rows

*Removing NA*

``` r
fli_small %>%
  specify(response = arr_delay) %>% # alt: arr_delay ~ NULL (or arr_delay ~ 1)
  hypothesize(null = "point", sd = 40) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "sd", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate     stat
    ##        <int>    <dbl>
    ##  1         1 38.82159
    ##  2         2 37.75836
    ##  3         3 36.87100
    ##  4         4 43.16722
    ##  5         5 35.60416
    ##  6         6 29.51281
    ##  7         7 38.17352
    ##  8         8 37.11360
    ##  9         9 27.87567
    ## 10        10 36.05444
    ## # ... with 4,990 more rows

One categorical (2 level) variable

``` r
fli_small %>%
  specify(response = half_year) %>% # alt: half_year ~ NULL (or half_year ~ 1)
  hypothesize(null = "point", p = c("h1" = .45, "h2" = 0.55)) %>% 
  generate(reps = params$rep_times, type = "simulate") %>% 
  calculate(stat = "prop")
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##       <fctr> <dbl>
    ##  1         1 0.495
    ##  2         2 0.475
    ##  3         3 0.375
    ##  4         4 0.485
    ##  5         5 0.405
    ##  6         6 0.455
    ##  7         7 0.445
    ##  8         8 0.430
    ##  9         9 0.480
    ## 10        10 0.500
    ## # ... with 4,990 more rows

Two categorical (2 level) variables

``` r
fli_small %>%
  specify(half_year ~ day_hour) %>% # alt: response = half_year, explanatory = vs
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in props")
```

    ## # A tibble: 5,000 x 2
    ##    replicate         stat
    ##        <int>        <dbl>
    ##  1         1  0.008975928
    ##  2         2 -0.067115463
    ##  3         3 -0.026315789
    ##  4         4 -0.175030600
    ##  5         5 -0.061199510
    ##  6         6  0.067115463
    ##  7         7  0.038147695
    ##  8         8 -0.005915953
    ##  9         9  0.058547532
    ## 10        10 -0.023459812
    ## # ... with 4,990 more rows

One categorical (&gt;2 level) - GoF

``` r
fli_small %>%
  specify(origin ~ NULL) %>% # alt: response = day_hour
  hypothesize(null = "point", 
              p = c("EWR" = .3, "JFK" = .4, "LGA" = .3)) %>%
  generate(reps = params$rep_times, type = "simulate") %>%
  calculate(stat = "Chisq")
```

    ## # A tibble: 5,000 x 2
    ##    replicate       stat
    ##       <fctr>      <dbl>
    ##  1         1 1.42916667
    ##  2         2 1.58333333
    ##  3         3 1.02916667
    ##  4         4 2.02916667
    ##  5         5 0.09583333
    ##  6         6 1.02916667
    ##  7         7 1.02916667
    ##  8         8 1.28333333
    ##  9         9 4.80000000
    ## 10        10 0.11666667
    ## # ... with 4,990 more rows

Two categorical (&gt;2 level) variables

``` r
fli_small %>%
  specify(origin ~ carrier) %>% # alt: response = origin, explanatory = carrier
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "Chisq")
```

    ## # A tibble: 5,000 x 2
    ##    replicate     stat
    ##        <int>    <dbl>
    ##  1         1 20.56815
    ##  2         2 31.53947
    ##  3         3 31.55485
    ##  4         4 32.19428
    ##  5         5 32.74599
    ##  6         6 24.42849
    ##  7         7 36.44255
    ##  8         8 25.82679
    ##  9         9 34.01567
    ## 10        10 19.44962
    ## # ... with 4,990 more rows

One numerical variable one categorical (2 levels) (diff in means)

*With NAs*

``` r
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in means")
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1    NA
    ##  2         2    NA
    ##  3         3    NA
    ##  4         4    NA
    ##  5         5    NA
    ##  6         6    NA
    ##  7         7    NA
    ##  8         8    NA
    ##  9         9    NA
    ## 10        10    NA
    ## # ... with 4,990 more rows

*Without NAs*

``` r
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in means", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate       stat
    ##        <int>      <dbl>
    ##  1         1 -4.9283557
    ##  2         2  6.9321123
    ##  3         3 -1.6480429
    ##  4         4 -6.0657257
    ##  5         5 -2.9017586
    ##  6         6 12.8712500
    ##  7         7  6.9412914
    ##  8         8 -3.8108511
    ##  9         9  1.0767247
    ## 10        10  0.8409588
    ## # ... with 4,990 more rows

One numerical variable one categorical (2 levels) (diff in medians)

``` r
fli_small %>%
  specify(arr_delay ~ half_year) %>% # alt: response = arr_delay, explanatory = half_year
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "diff in medians", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1 -11.5
    ##  2         2   1.5
    ##  3         3   8.0
    ##  4         4   6.0
    ##  5         5   0.5
    ##  6         6   1.0
    ##  7         7   3.0
    ##  8         8  -3.0
    ##  9         9   5.0
    ## 10        10  -7.0
    ## # ... with 4,990 more rows

One numerical one categorical (&gt;2 levels) - ANOVA

``` r
fli_small %>%
  specify(arr_delay ~ origin) %>% # alt: response = arr_delay, explanatory = day_hour
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "F")
```

    ## # A tibble: 5,000 x 2
    ##    replicate       stat
    ##        <int>      <dbl>
    ##  1         1 3.38671298
    ##  2         2 1.61273836
    ##  3         3 5.77012952
    ##  4         4 0.14820360
    ##  5         5 0.62427874
    ##  6         6 0.96159173
    ##  7         7 0.08827497
    ##  8         8 0.90046796
    ##  9         9 1.62193373
    ## 10        10 0.20834682
    ## # ... with 4,990 more rows

Two numerical vars - SLR

``` r
fli_small %>%
  specify(arr_delay ~ dep_delay) %>% # alt: response = arr_delay, explanatory = dep_delay
  hypothesize(null = "independence") %>%
  generate(reps = params$rep_times, type = "permute") %>%
  calculate(stat = "slope")
```

    ## # A tibble: 5,000 x 2
    ##    replicate        stat
    ##        <int>       <dbl>
    ##  1         1 -0.13353560
    ##  2         2  0.10817357
    ##  3         3 -0.05907885
    ##  4         4 -0.01935851
    ##  5         5 -0.08536161
    ##  6         6 -0.01563617
    ##  7         7 -0.02351870
    ##  8         8 -0.00550564
    ##  9         9  0.01367293
    ## 10        10 -0.10836992
    ## # ... with 4,990 more rows

### Confidence intervals

One numerical (one mean)

``` r
fli_small %>%
  specify(response = arr_delay) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "mean", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate        stat
    ##        <int>       <dbl>
    ##  1         1  3.88421053
    ##  2         2  5.13989637
    ##  3         3 -0.06091371
    ##  4         4  4.85492228
    ##  5         5  3.41025641
    ##  6         6  4.02538071
    ##  7         7  2.86458333
    ##  8         8  0.08163265
    ##  9         9  1.18556701
    ## 10        10  7.91836735
    ## # ... with 4,990 more rows

One numerical (one median)

``` r
fli_small %>%
  specify(response = arr_delay) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "median", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1  -2.0
    ##  2         2  -8.0
    ##  3         3  -9.0
    ##  4         4  -6.0
    ##  5         5  -6.0
    ##  6         6 -10.0
    ##  7         7  -6.5
    ##  8         8 -10.0
    ##  9         9  -9.0
    ## 10        10  -8.5
    ## # ... with 4,990 more rows

One categorical (one proportion)

``` r
fli_small %>%
  specify(response = half_year) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "prop", success = "h2")
```

    ## # A tibble: 5,000 x 2
    ##    replicate  stat
    ##        <int> <dbl>
    ##  1         1 0.455
    ##  2         2 0.535
    ##  3         3 0.530
    ##  4         4 0.520
    ##  5         5 0.530
    ##  6         6 0.470
    ##  7         7 0.565
    ##  8         8 0.485
    ##  9         9 0.575
    ## 10        10 0.525
    ## # ... with 4,990 more rows

One numerical variable one categorical (2 levels) (diff in means)

``` r
fli_small %>%
  specify(arr_delay ~ half_year) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "diff in means", na.rm = TRUE)
```

    ## # A tibble: 5,000 x 2
    ##    replicate        stat
    ##        <int>       <dbl>
    ##  1         1  -4.8809329
    ##  2         2  -3.5407626
    ##  3         3  -3.2812500
    ##  4         4  -4.2598028
    ##  5         5  -3.7680851
    ##  6         6 -16.0719626
    ##  7         7  -4.8697763
    ##  8         8   8.1491402
    ##  9         9  -0.3682598
    ## 10        10  -2.4458940
    ## # ... with 4,990 more rows

Two categorical variables (diff in proportions)

``` r
fli_small %>%
  specify(half_year ~ day_hour) %>%
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "diff in props")
```

    ## # A tibble: 5,000 x 2
    ##    replicate        stat
    ##        <int>       <dbl>
    ##  1         1  0.04880488
    ##  2         2 -0.10064412
    ##  3         3  0.03613418
    ##  4         4 -0.06369955
    ##  5         5  0.09285196
    ##  6         6  0.17685917
    ##  7         7  0.04243179
    ##  8         8  0.10385878
    ##  9         9  0.18441664
    ## 10        10 -0.02259651
    ## # ... with 4,990 more rows

Two numerical vars - SLR

``` r
fli_small %>%
  specify(arr_delay ~ dep_delay) %>% 
  generate(reps = params$rep_times, type = "bootstrap") %>%
  calculate(stat = "slope")
```

    ## # A tibble: 5,000 x 2
    ##    replicate      stat
    ##        <int>     <dbl>
    ##  1         1 1.0232583
    ##  2         2 1.0165367
    ##  3         3 1.0401114
    ##  4         4 0.9407880
    ##  5         5 1.0439949
    ##  6         6 1.0187435
    ##  7         7 1.0355809
    ##  8         8 0.9246398
    ##  9         9 0.9617520
    ## 10        10 0.9267954
    ## # ... with 4,990 more rows
