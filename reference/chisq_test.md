# Tidy chi-squared test

A tidier version of
[chisq.test()](https://rdrr.io/r/stats/chisq.test.html) for goodness of
fit tests and tests of independence.

## Usage

``` r
chisq_test(x, formula, response = NULL, explanatory = NULL, ...)
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

- ...:

  Additional arguments for
  [chisq.test()](https://rdrr.io/r/stats/chisq.test.html).

## See also

Other wrapper functions:
[`chisq_stat()`](https://infer.tidymodels.org/reference/chisq_stat.md),
[`observe()`](https://infer.tidymodels.org/reference/observe.md),
[`prop_test()`](https://infer.tidymodels.org/reference/prop_test.md),
[`t_stat()`](https://infer.tidymodels.org/reference/t_stat.md),
[`t_test()`](https://infer.tidymodels.org/reference/t_test.md)

## Examples

``` r
# chi-squared test of independence for college completion
# status depending on one's self-identified income class
chisq_test(gss, college ~ finrela)
#> Warning: Chi-squared approximation may be incorrect
#> # A tibble: 1 × 3
#>   statistic chisq_df   p_value
#>       <dbl>    <int>     <dbl>
#> 1      30.7        5 0.0000108

# chi-squared goodness of fit test on whether self-identified
# income class follows a uniform distribution
chisq_test(gss,
           response = finrela,
           p = c("far below average" = 1/6,
                 "below average" = 1/6,
                 "average" = 1/6,
                 "above average" = 1/6,
                 "far above average" = 1/6,
                 "DK" = 1/6))
#> # A tibble: 1 × 3
#>   statistic chisq_df   p_value
#>       <dbl>    <dbl>     <dbl>
#> 1      488.        5 3.13e-103
```
