# Compute confidence interval

Compute a confidence interval around a summary statistic. Both
simulation-based and theoretical methods are supported, though only
`type = "se"` is supported for theoretical methods.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

## Usage

``` r
get_confidence_interval(x, level = 0.95, type = NULL, point_estimate = NULL)

get_ci(x, level = 0.95, type = NULL, point_estimate = NULL)
```

## Arguments

- x:

  A distribution. For simulation-based inference, a data frame
  containing a distribution of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)d
  statistics or
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md)ted
  coefficient estimates. This object should have been passed to
  [`generate()`](https://infer.tidymodels.org/reference/generate.md)
  before being supplied or
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  to [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md). For
  theory-based inference, output of
  [`assume()`](https://infer.tidymodels.org/reference/assume.md).
  Distributions for confidence intervals do not require a null
  hypothesis via
  [`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md).

- level:

  A numerical value between 0 and 1 giving the confidence level. Default
  value is 0.95.

- type:

  A string giving which method should be used for creating the
  confidence interval. The default is `"percentile"` with `"se"`
  corresponding to (multiplier \* standard error) and `"bias-corrected"`
  for bias-corrected interval as other options.

- point_estimate:

  A data frame containing the observed statistic (in a
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)-based
  workflow) or observed fit (in a
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md)-based
  workflow). This object is likely the output of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  or [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md) and
  need not to have been passed to
  [`generate()`](https://infer.tidymodels.org/reference/generate.md).
  Set to `NULL` by default. Must be provided if `type` is `"se"` or
  `"bias-corrected"`.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing the following columns:

- `term`: The explanatory variable (or intercept) in question. Only
  supplied if the input had been previously passed to
  [`fit()`](https://infer.tidymodels.org/reference/fit.infer.md).

- `lower_ci`, `upper_ci`: The lower and upper bounds of the confidence
  interval, respectively.

## Details

A null hypothesis is not required to compute a confidence interval.
However, including
[`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md)
in a pipeline leading to `get_confidence_interval()` will not break
anything. This can be useful when computing a confidence interval using
the same distribution used to compute a p-value.

Theoretical confidence intervals (i.e. calculated by supplying the
output of [`assume()`](https://infer.tidymodels.org/reference/assume.md)
to the `x` argument) require that the point estimate lies on the scale
of the data. The distribution defined in
[`assume()`](https://infer.tidymodels.org/reference/assume.md) will be
recentered and rescaled to align with the point estimate, as can be
shown in the output of
[`visualize()`](https://infer.tidymodels.org/reference/visualize.md)
when paired with
[`shade_confidence_interval()`](https://infer.tidymodels.org/reference/shade_confidence_interval.md).
Confidence intervals are implemented for the following distributions and
point estimates:

- `distribution = "t"`: `point_estimate` should be the output of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  with `stat = "mean"` or `stat = "diff in means"`

- `distribution = "z"`: `point_estimate` should be the output of
  [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  with `stat = "prop"` or `stat = "diff in props"`

## Aliases

`get_ci()` is an alias of `get_confidence_interval()`.
[`conf_int()`](https://infer.tidymodels.org/reference/deprecated.md) is
a deprecated alias of `get_confidence_interval()`.

## See also

Other auxillary functions:
[`get_p_value()`](https://infer.tidymodels.org/reference/get_p_value.md)

## Examples

``` r
boot_dist <- gss |>
  # We're interested in the number of hours worked per week
  specify(response = hours) |>
  # Generate bootstrap samples
  generate(reps = 1000, type = "bootstrap") |>
  # Calculate mean of each bootstrap sample
  calculate(stat = "mean")

boot_dist |>
  # Calculate the confidence interval around the point estimate
  get_confidence_interval(
    # At the 95% confidence level; percentile method
    level = 0.95
  )
#> # A tibble: 1 × 2
#>   lower_ci upper_ci
#>      <dbl>    <dbl>
#> 1     40.2     42.7

# for type = "se" or type = "bias-corrected" we need a point estimate
sample_mean <- gss |>
  specify(response = hours) |>
  calculate(stat = "mean")

boot_dist |>
  get_confidence_interval(
    point_estimate = sample_mean,
    # At the 95% confidence level
    level = 0.95,
    # Using the standard error method
    type = "se"
  )
#> # A tibble: 1 × 2
#>   lower_ci upper_ci
#>      <dbl>    <dbl>
#> 1     40.1     42.7

# using a theoretical distribution -----------------------------------

# define a sampling distribution
sampling_dist <- gss |>
  specify(response = hours) |>
  assume("t")

# get the confidence interval---note that the
# point estimate is required here
get_confidence_interval(
  sampling_dist,
  level = .95,
  point_estimate = sample_mean
)
#> # A tibble: 1 × 2
#>   lower_ci upper_ci
#>      <dbl>    <dbl>
#> 1     40.1     42.7

# using a model fitting workflow -----------------------

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
#>    replicate term           estimate
#>        <int> <chr>             <dbl>
#>  1         1 intercept     42.0     
#>  2         1 age           -0.00835 
#>  3         1 collegedegree -0.790   
#>  4         2 intercept     41.5     
#>  5         2 age           -0.000968
#>  6         2 collegedegree -0.329   
#>  7         3 intercept     41.4     
#>  8         3 age            0.0131  
#>  9         3 collegedegree -1.50    
#> 10         4 intercept     42.0     
#> # ℹ 290 more rows

get_confidence_interval(
  null_fits,
  point_estimate = observed_fit,
  level = .95
)
#> # A tibble: 3 × 3
#>   term          lower_ci upper_ci
#>   <chr>            <dbl>    <dbl>
#> 1 age            -0.0846   0.0856
#> 2 collegedegree  -2.10     2.81  
#> 3 intercept      38.1     44.7   

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
