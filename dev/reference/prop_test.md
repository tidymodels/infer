# Tidy proportion test

A tidier version of
[prop.test()](https://rdrr.io/r/stats/prop.test.html) for equal or given
proportions.

## Usage

``` r
prop_test(
  x,
  formula,
  response = NULL,
  explanatory = NULL,
  p = NULL,
  order = NULL,
  alternative = "two-sided",
  conf_int = TRUE,
  conf_level = 0.95,
  success = NULL,
  correct = NULL,
  z = FALSE,
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

- p:

  A numeric vector giving the hypothesized null proportion of success
  for each group.

- order:

  A string vector specifying the order in which the proportions should
  be subtracted, where `order = c("first", "second")` means
  `"first" - "second"`. Ignored for one-sample tests, and optional for
  two sample tests.

- alternative:

  Character string giving the direction of the alternative hypothesis.
  Options are `"two-sided"` (default), `"greater"`, or `"less"`. Only
  used when testing the null that a single proportion equals a given
  value, or that two proportions are equal; ignored otherwise.

- conf_int:

  A logical value for whether to include the confidence interval or not.
  `TRUE` by default.

- conf_level:

  A numeric value between 0 and 1. Default value is 0.95.

- success:

  The level of `response` that will be considered a success, as a
  string. Only used when testing the null that a single proportion
  equals a given value, or that two proportions are equal; ignored
  otherwise.

- correct:

  A logical indicating whether Yates' continuity correction should be
  applied where possible. If `z = TRUE`, the `correct` argument will be
  overwritten as `FALSE`. Otherwise defaults to `correct = TRUE`.

- z:

  A logical value for whether to report the statistic as a standard
  normal deviate or a Pearson's chi-square statistic. \\z^2\\ is
  distributed chi-square with 1 degree of freedom, though note that the
  user will likely need to turn off Yates' continuity correction by
  setting `correct = FALSE` to see this connection.

- ...:

  Additional arguments for
  [prop.test()](https://rdrr.io/r/stats/prop.test.html).

## Details

When testing with an explanatory variable with more than two levels, the
`order` argument as used in the package is no longer well-defined. The
function will thus raise a warning and ignore the value if supplied a
non-NULL `order` argument.

The columns present in the output depend on the output of both
[`prop.test()`](https://rdrr.io/r/stats/prop.test.html) and
[`broom::glance.htest()`](https://broom.tidymodels.org/reference/tidy.htest.html).
See the latter's documentation for column definitions; columns have been
renamed with the following mapping:

- `chisq_df` = `parameter`

- `p_value` = `p.value`

- `lower_ci` = `conf.low`

- `upper_ci` = `conf.high`

## See also

Other wrapper functions:
[`chisq_stat()`](https://infer.tidymodels.org/dev/reference/chisq_stat.md),
[`chisq_test()`](https://infer.tidymodels.org/dev/reference/chisq_test.md),
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md),
[`t_stat()`](https://infer.tidymodels.org/dev/reference/t_stat.md),
[`t_test()`](https://infer.tidymodels.org/dev/reference/t_test.md)

## Examples

``` r
# two-sample proportion test for difference in proportions of
# college completion by respondent sex
prop_test(gss,
          college ~ sex,
          order = c("female", "male"))
#> # A tibble: 1 × 6
#>   statistic chisq_df p_value alternative lower_ci upper_ci
#>       <dbl>    <dbl>   <dbl> <chr>          <dbl>    <dbl>
#> 1 0.0000204        1   0.996 two.sided    -0.0918   0.0834

# one-sample proportion test for hypothesized null
# proportion of college completion of .2
prop_test(gss,
          college ~ NULL,
          p = .2)
#> # A tibble: 1 × 4
#>   statistic chisq_df   p_value alternative
#>       <dbl>    <int>     <dbl> <chr>      
#> 1      636.        1 2.98e-140 two.sided  

# report as a z-statistic rather than chi-square
# and specify the success level of the response
prop_test(gss,
          college ~ NULL,
          success = "degree",
          p = .2,
          z = TRUE)
#> # A tibble: 1 × 3
#>   statistic  p_value alternative
#>       <dbl>    <dbl> <chr>      
#> 1      8.27 1.30e-16 two.sided  
```
