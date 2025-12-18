# Compute p-value

Compute a p-value from a null distribution and observed statistic.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

## Usage

``` r
get_p_value(x, obs_stat, direction)

# Default S3 method
get_p_value(x, obs_stat, direction)

get_pvalue(x, obs_stat, direction)

# S3 method for class 'infer_dist'
get_p_value(x, obs_stat, direction)
```

## Arguments

- x:

  A null distribution. For simulation-based inference, a data frame
  containing a distribution of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)d
  statistics or
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md)ted
  coefficient estimates. This object should have been passed to
  [`generate()`](https://infer.tidymodels.org/reference/generate.md)
  before being supplied or
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  to [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md). For
  theory-based inference, the output of
  [`assume()`](https://infer.tidymodels.org/reference/assume.md).

- obs_stat:

  A data frame containing the observed statistic (in a
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)-based
  workflow) or observed fit (in a
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md)-based
  workflow). This object is likely the output of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  or [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md) and
  need not to have been passed to
  [`generate()`](https://infer.tidymodels.org/reference/generate.md).

- direction:

  A character string. Options are `"less"`, `"greater"`, or
  `"two-sided"`. Can also use `"left"`, `"right"`, `"both"`,
  `"two_sided"`, or `"two sided"`, `"two.sided"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing the following columns:

- `term`: The explanatory variable (or intercept) in question. Only
  supplied if the input had been previously passed to
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md).

- `p_value`: A value in \[0, 1\] giving the probability that a
  statistic/coefficient as or more extreme than the observed
  statistic/coefficient would occur if the null hypothesis were true.

## Aliases

`get_pvalue()` is an alias of `get_p_value()`. `p_value` is a deprecated
alias of `get_p_value()`.

## Zero p-value

Though a true p-value of 0 is impossible, `get_p_value()` may return 0
in some cases. This is due to the simulation-based nature of the {infer}
package; the output of this function is an approximation based on the
number of `reps` chosen in the
[`generate()`](https://infer.tidymodels.org/reference/generate.md) step.
When the observed statistic is very unlikely given the null hypothesis,
and only a small number of `reps` have been generated to form a null
distribution, it is possible that the observed statistic will be more
extreme than every test statistic generated to form the null
distribution, resulting in an approximate p-value of 0. In this case,
the true p-value is a small value likely less than `3/reps` (based on a
poisson approximation).

In the case that a p-value of zero is reported, a warning message will
be raised to caution the user against reporting a p-value exactly equal
to 0.

## See also

Other auxillary functions:
[`get_confidence_interval()`](https://infer.tidymodels.org/reference/get_confidence_interval.md)

## Examples

``` r
# using a simulation-based null distribution ------------------------------

# find the point estimate---mean number of hours worked per week
point_estimate <- gss |>
  specify(response = hours) |>
  calculate(stat = "mean")

# starting with the gss dataset
gss |>
  # ...we're interested in the number of hours worked per week
  specify(response = hours) |>
  # hypothesizing that the mean is 40
  hypothesize(null = "point", mu = 40) |>
  # generating data points for a null distribution
  generate(reps = 1000, type = "bootstrap") |>
  # finding the null distribution
  calculate(stat = "mean") |>
  get_p_value(obs_stat = point_estimate, direction = "two-sided")
#> # A tibble: 1 × 1
#>   p_value
#>     <dbl>
#> 1   0.032

# using a theoretical null distribution -----------------------------------

# calculate the observed statistic
obs_stat <- gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  calculate(stat = "t")

# define a null distribution
null_dist <- gss |>
  specify(response = hours) |>
  assume("t")

# calculate a p-value
get_p_value(null_dist, obs_stat, direction = "both")
#> # A tibble: 1 × 1
#>   p_value
#>     <dbl>
#> 1  0.0376

# using a model fitting workflow -----------------------------------------

# fit a linear model predicting number of hours worked per
# week using respondent age and degree status.
observed_fit <- gss |>
  specify(hours ~ age + college) |>
  fit()

observed_fit
#> # A tibble: 3 × 2
#>   term          estimate
#>   <chr>            <dbl>
#> 1 intercept     40.6    
#> 2 age            0.00596
#> 3 collegedegree  1.53   

# fit 100 models to resamples of the gss dataset, where the response
# `hours` is permuted in each. note that this code is the same as
# the above except for the addition of the `generate` step.
null_fits <- gss |>
  specify(hours ~ age + college) |>
  hypothesize(null = "independence") |>
  generate(reps = 100, type = "permute") |>
  fit()

null_fits
#> # A tibble: 300 × 3
#> # Groups:   replicate [100]
#>    replicate term          estimate
#>        <int> <chr>            <dbl>
#>  1         1 intercept      43.8   
#>  2         1 age            -0.0588
#>  3         1 collegedegree  -0.178 
#>  4         2 intercept      39.8   
#>  5         2 age             0.0345
#>  6         2 collegedegree   0.554 
#>  7         3 intercept      42.7   
#>  8         3 age            -0.0426
#>  9         3 collegedegree   1.23  
#> 10         4 intercept      42.6   
#> # ℹ 290 more rows

get_p_value(null_fits, obs_stat = observed_fit, direction = "two-sided")
#> # A tibble: 3 × 2
#>   term          p_value
#>   <chr>           <dbl>
#> 1 age              0.94
#> 2 collegedegree    0.24
#> 3 intercept        0.7 

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
