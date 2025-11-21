# Tidy chi-squared test statistic

@description

## Usage

``` r
chisq_stat(x, formula, response = NULL, explanatory = NULL, ...)
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

## Details

A shortcut wrapper function to get the observed test statistic for a
chisq test. Uses
[chisq.test()](https://rdrr.io/r/stats/chisq.test.html), which applies a
continuity correction. This function has been deprecated in favor of the
more general
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md).

## See also

Other wrapper functions:
[`chisq_test()`](https://infer.tidymodels.org/dev/reference/chisq_test.md),
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md),
[`prop_test()`](https://infer.tidymodels.org/dev/reference/prop_test.md),
[`t_stat()`](https://infer.tidymodels.org/dev/reference/t_stat.md),
[`t_test()`](https://infer.tidymodels.org/dev/reference/t_test.md)

Other functions for calculating observed statistics:
[`observe()`](https://infer.tidymodels.org/dev/reference/observe.md),
[`t_stat()`](https://infer.tidymodels.org/dev/reference/t_stat.md)

## Examples

``` r
# chi-squared test statistic for test of independence
# of college completion status depending and one's
# self-identified income class
chisq_stat(gss, college ~ finrela)
#> Warning: `chisq_stat()` was deprecated in infer 1.0.0.
#> ℹ Please use `observe()` instead.
#> X-squared 
#>  30.68252 

# chi-squared test statistic for a goodness of fit
# test on whether self-identified income class
# follows a uniform distribution
chisq_stat(gss,
           response = finrela,
           p = c("far below average" = 1/6,
                 "below average" = 1/6,
                 "average" = 1/6,
                 "above average" = 1/6,
                 "far above average" = 1/6,
                 "DK" = 1/6))
#> X-squared 
#>   487.984 
```
