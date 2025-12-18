# Declare a null hypothesis

Declare a null hypothesis about variables selected in
[`specify()`](https://infer.tidymodels.org/reference/specify.md).

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

## Usage

``` r
hypothesize(x, null, p = NULL, mu = NULL, med = NULL, sigma = NULL)

hypothesise(x, null, p = NULL, mu = NULL, med = NULL, sigma = NULL)
```

## Arguments

- x:

  A data frame that can be coerced into a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

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

## Value

A tibble containing the response (and explanatory, if specified)
variable data with parameter information stored as well.

## See also

Other core functions:
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md),
[`generate()`](https://infer.tidymodels.org/reference/generate.md),
[`specify()`](https://infer.tidymodels.org/reference/specify.md)

## Examples

``` r
# hypothesize independence of two variables
gss |>
 specify(college ~ partyid, success = "degree") |>
 hypothesize(null = "independence")
#> Dropping unused factor levels DK from the supplied explanatory
#> variable 'partyid'.
#> Response: college (factor)
#> Explanatory: partyid (factor)
#> Null Hypoth...
#> # A tibble: 500 × 2
#>    college   partyid
#>    <fct>     <fct>  
#>  1 degree    ind    
#>  2 no degree rep    
#>  3 degree    ind    
#>  4 no degree ind    
#>  5 degree    rep    
#>  6 no degree rep    
#>  7 no degree dem    
#>  8 degree    ind    
#>  9 degree    rep    
#> 10 no degree dem    
#> # ℹ 490 more rows

# hypothesize a mean number of hours worked per week of 40
gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40)
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 500 × 1
#>    hours
#>    <dbl>
#>  1    50
#>  2    31
#>  3    40
#>  4    40
#>  5    40
#>  6    53
#>  7    32
#>  8    20
#>  9    40
#> 10    40
#> # ℹ 490 more rows

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
