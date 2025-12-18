# Tidy t-test

A tidier version of [t.test()](https://rdrr.io/r/stats/t.test.html) for
two sample tests.

## Usage

``` r
t_test(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  order = NULL,
  alternative = "two-sided",
  mu = 0,
  conf_int = TRUE,
  conf_level = 0.95,
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

- order:

  A string vector of specifying the order in which the levels of the
  explanatory variable should be ordered for subtraction, where
  `order = c("first", "second")` means `("first" - "second")`.

- alternative:

  Character string giving the direction of the alternative hypothesis.
  Options are `"two-sided"` (default), `"greater"`, or `"less"`.

- mu:

  A numeric value giving the hypothesized null mean value for a one
  sample test and the hypothesized difference for a two sample test.

- conf_int:

  A logical value for whether to include the confidence interval or not.
  `TRUE` by default.

- conf_level:

  A numeric value between 0 and 1. Default value is 0.95.

- ...:

  For passing in other arguments to
  [t.test()](https://rdrr.io/r/stats/t.test.html).

## See also

Other wrapper functions:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`chisq_test()`](https://infer.tidymodels.org/reference/chisq_test.md),
[`observe()`](https://infer.tidymodels.org/reference/observe.md),
[`prop_test()`](https://infer.tidymodels.org/reference/prop_test.md),
[`t_stat()`](https://infer.tidymodels.org/reference/t_stat.md)

## Examples

``` r
library(tidyr)

# t test for number of hours worked per week
# by college degree status
gss |>
   tidyr::drop_na(college) |>
   t_test(formula = hours ~ college,
      order = c("degree", "no degree"),
      alternative = "two-sided")
#> # A tibble: 1 × 7
#>   statistic  t_df p_value alternative estimate lower_ci upper_ci
#>       <dbl> <dbl>   <dbl> <chr>          <dbl>    <dbl>    <dbl>
#> 1      1.12  366.   0.264 two.sided       1.54    -1.16     4.24

# see vignette("infer") for more explanation of the
# intuition behind the infer package, and vignette("t_test")
# for more examples of t-tests using infer
```
