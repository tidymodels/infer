# Tidy t-test statistic

A shortcut wrapper function to get the observed test statistic for a t
test. This function has been deprecated in favor of the more general
[`observe()`](https://infer.tidymodels.org/reference/observe.md).

## Usage

``` r
t_stat(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  order = NULL,
  alternative = "two-sided",
  mu = 0,
  conf_int = FALSE,
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

  Pass in arguments to infer functions.

## See also

Other wrapper functions:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`chisq_test()`](https://infer.tidymodels.org/reference/chisq_test.md),
[`observe()`](https://infer.tidymodels.org/reference/observe.md),
[`prop_test()`](https://infer.tidymodels.org/reference/prop_test.md),
[`t_test()`](https://infer.tidymodels.org/reference/t_test.md)

Other functions for calculating observed statistics:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`observe()`](https://infer.tidymodels.org/reference/observe.md)

## Examples

``` r
library(tidyr)

# t test statistic for true mean number of hours worked
# per week of 40
gss |>
   t_stat(response = hours, mu = 40)
#> Warning: `t_stat()` was deprecated in infer 1.0.0.
#> ℹ Please use `observe()` instead.
#>        t 
#> 2.085191 

# t test statistic for number of hours worked per week
# by college degree status
gss |>
   tidyr::drop_na(college) |>
   t_stat(formula = hours ~ college,
      order = c("degree", "no degree"),
      alternative = "two-sided")
#>       t 
#> 1.11931 
```
