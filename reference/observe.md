# Calculate observed statistics

This function is a wrapper that calls
[`specify()`](https://infer.tidymodels.org/reference/specify.md),
[`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md),
and [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
consecutively that can be used to calculate observed statistics from
data.
[`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md)
will only be called if a point null hypothesis parameter is supplied.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

## Usage

``` r
observe(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  success = NULL,
  null = NULL,
  p = NULL,
  mu = NULL,
  med = NULL,
  sigma = NULL,
  stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means",
    "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z",
    "ratio of props", "odds ratio"),
  order = NULL,
  ...
)
```

## Arguments

- x:

  A data frame that can be coerced into a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

- formula:

  A formula with the response variable on the left and the explanatory
  on the right. Alternatively, a `response` and `explanatory` argument
  can be supplied.

- response:

  The variable name in `x` that will serve as the response. This is an
  alternative to using the `formula` argument.

- explanatory:

  The variable name in `x` that will serve as the explanatory variable.
  This is an alternative to using the formula argument.

- success:

  The level of `response` that will be considered a success, as a
  string. Needed for inference on one proportion, a difference in
  proportions, and corresponding z stats.

- null:

  The null hypothesis. Options include `"independence"`, `"point"`, and
  `"paired independence"`.

  - `independence`: Should be used with both a `response` and
    `explanatory` variable. Indicates that the values of the specified
    `response` variable are independent of the associated values in
    `explanatory`.

  - `point`: Should be used with only a `response` variable. Indicates
    that a point estimate based on the values in `response` is
    associated with a parameter. Sometimes requires supplying one of
    `p`, `mu`, `med`, or `sigma`.

  - `paired independence`: Should be used with only a `response`
    variable giving the pre-computed difference between paired
    observations. Indicates that the order of subtraction between paired
    values does not affect the resulting distribution.

- p:

  The true proportion of successes (a number between 0 and 1). To be
  used with point null hypotheses when the specified response variable
  is categorical.

- mu:

  The true mean (any numerical value). To be used with point null
  hypotheses when the specified response variable is continuous.

- med:

  The true median (any numerical value). To be used with point null
  hypotheses when the specified response variable is continuous.

- sigma:

  The true standard deviation (any numerical value). To be used with
  point null hypotheses.

- stat:

  A string giving the type of the statistic to calculate or a function
  that takes in a replicate of `x` and returns a scalar value. Current
  options include `"mean"`, `"median"`, `"sum"`, `"sd"`, `"prop"`,
  `"count"`, `"diff in means"`, `"diff in medians"`, `"diff in props"`,
  `"Chisq"` (or `"chisq"`), `"F"` (or `"f"`), `"t"`, `"z"`,
  `"ratio of props"`, `"slope"`, `"odds ratio"`, `"ratio of means"`, and
  `"correlation"`. `infer` only supports theoretical tests on one or two
  means via the `"t"` distribution and one or two proportions via the
  `"z"`. See the "Arbitrary test statistics" section below for more on
  how to define a custom statistic.

- order:

  A string vector of specifying the order in which the levels of the
  explanatory variable should be ordered for subtraction (or division
  for ratio-based statistics), where `order = c("first", "second")`
  means `("first" - "second")`, or the analogue for ratios. Needed for
  inference on difference in means, medians, proportions, ratios, t, and
  z statistics.

- ...:

  To pass options like `na.rm = TRUE` into functions like
  [mean()](https://rdrr.io/r/base/mean.html),
  [sd()](https://rdrr.io/r/stats/sd.html), etc. Can also be used to
  supply hypothesized null values for the `"t"` statistic or additional
  arguments to
  [`stats::chisq.test()`](https://rdrr.io/r/stats/chisq.test.html).

## Value

A 1-column tibble containing the calculated statistic `stat`.

## Arbitrary test statistics

In addition to the pre-implemented statistics documented in `stat`,
users can supply an arbitrary test statistic by supplying a function to
the `stat` argument.

The function should have arguments `stat(x, order, ...)`, where `x` is
one replicate's worth of `x`. The `order` argument and ellipses will be
supplied directly to the `stat` function. Internally,
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
will split `x` up into data frames by replicate and pass them one-by-one
to the supplied `stat`. For example, to implement `stat = "mean"` as a
function, one could write:

    stat_mean <- function(x, order, ...) {mean(x$hours)}
    obs_mean <-
      gss %>%
      specify(response = hours) %>%
      calculate(stat = stat_mean)

    set.seed(1)
    null_dist_mean <-
      gss %>%
      specify(response = hours) %>%
      hypothesize(null = "point", mu = 40) %>%
      generate(reps = 5, type = "bootstrap") %>%
      calculate(stat = stat_mean)

Note that the same `stat_mean` function is supplied to both
[`generate()`](https://infer.tidymodels.org/reference/generate.md)d and
non-[`generate()`](https://infer.tidymodels.org/reference/generate.md)d
infer objects–no need to implement support for grouping by `replicate`
yourself.

## See also

Other wrapper functions:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`chisq_test()`](https://infer.tidymodels.org/reference/chisq_test.md),
[`prop_test()`](https://infer.tidymodels.org/reference/prop_test.md),
[`t_stat()`](https://infer.tidymodels.org/reference/t_stat.md),
[`t_test()`](https://infer.tidymodels.org/reference/t_test.md)

Other functions for calculating observed statistics:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`t_stat()`](https://infer.tidymodels.org/reference/t_stat.md)

## Examples

``` r
# calculating the observed mean number of hours worked per week
gss |>
  observe(hours ~ NULL, stat = "mean")
#> Response: hours (numeric)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1  41.4

# equivalently, calculating the same statistic with the core verbs
gss |>
  specify(response = hours) |>
  calculate(stat = "mean")
#> Response: hours (numeric)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1  41.4

# calculating a t statistic for hypothesized mu = 40 hours worked/week
gss |>
  observe(hours ~ NULL, stat = "t", null = "point", mu = 40)
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1  2.09

# equivalently, calculating the same statistic with the core verbs
gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  calculate(stat = "t")
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1  2.09

# similarly for a difference in means in age based on whether
# the respondent has a college degree
observe(
  gss,
  age ~ college,
  stat = "diff in means",
  order = c("degree", "no degree")
)
#> Response: age (numeric)
#> Explanatory: college (factor)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1 0.941

# equivalently, calculating the same statistic with the core verbs
gss |>
  specify(age ~ college) |>
  calculate("diff in means", order = c("degree", "no degree"))
#> Response: age (numeric)
#> Explanatory: college (factor)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1 0.941

# for a more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
