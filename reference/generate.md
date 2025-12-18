# Generate resamples, permutations, or simulations

Generation creates a simulated distribution from
[`specify()`](https://infer.tidymodels.org/reference/specify.md). In the
context of confidence intervals, this is a bootstrap distribution based
on the result of
[`specify()`](https://infer.tidymodels.org/reference/specify.md). In the
context of hypothesis testing, this is a null distribution based on the
result of
[`specify()`](https://infer.tidymodels.org/reference/specify.md) and
`hypothesize().`

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

## Usage

``` r
generate(x, reps = 1, type = NULL, variables = !!response_expr(x), ...)
```

## Arguments

- x:

  A data frame that can be coerced into a
  [tibble](https://tibble.tidyverse.org/reference/tibble.html).

- reps:

  The number of resamples to generate.

- type:

  The method used to generate resamples of the observed data reflecting
  the null hypothesis. Currently one of `"bootstrap"`, `"permute"`, or
  `"draw"` (see below).

- variables:

  If `type = "permute"`, a set of unquoted column names in the data to
  permute (independently of each other). Defaults to only the response
  variable. Note that any derived effects that depend on these columns
  (e.g., interaction effects) will also be affected.

- ...:

  Currently ignored.

## Value

A tibble containing `reps` generated datasets, indicated by the
`replicate` column.

## Generation Types

The `type` argument determines the method used to create the null
distribution.

- `bootstrap`: A bootstrap sample will be drawn for each replicate,
  where a sample of size equal to the input sample size is drawn (with
  replacement) from the input sample data.

- `permute`: For each replicate, each input value will be randomly
  reassigned (without replacement) to a new output value in the sample.

- `draw`: A value will be sampled from a theoretical distribution with
  parameter `p` specified in
  [`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md)
  for each replicate. This option is currently only applicable for
  testing on one proportion. This generation type was previously called
  `"simulate"`, which has been superseded.

## Reproducibility

When using the infer package for research, or in other cases when exact
reproducibility is a priority, be sure the set the seed for R’s random
number generator. infer will respect the random seed specified in the
[`set.seed()`](https://rdrr.io/r/base/Random.html) function, returning
the same result when `generate()`ing data given an identical seed. For
instance, we can calculate the difference in mean `age` by `college`
degree status using the `gss` dataset from 10 versions of the `gss`
resampled with permutation using the following code.

    set.seed(1)

    gss |>
      specify(age ~ college) |>
      hypothesize(null = "independence") |>
      generate(reps = 5, type = "permute") |>
      calculate("diff in means", order = c("degree", "no degree"))

    ## Response: age (numeric)
    ## Explanatory: college (factor)
    ## Null Hypothesis: indepe...
    ## # A tibble: 5 x 2
    ##   replicate   stat
    ##       <int>  <dbl>
    ## 1         1 -0.531
    ## 2         2 -2.35
    ## 3         3  0.764
    ## 4         4  0.280
    ## 5         5  0.350

Setting the seed to the same value again and rerunning the same code
will produce the same result.

    # set the seed
    set.seed(1)

    gss |>
      specify(age ~ college) |>
      hypothesize(null = "independence") |>
      generate(reps = 5, type = "permute") |>
      calculate("diff in means", order = c("degree", "no degree"))

    ## Response: age (numeric)
    ## Explanatory: college (factor)
    ## Null Hypothesis: indepe...
    ## # A tibble: 5 x 2
    ##   replicate   stat
    ##       <int>  <dbl>
    ## 1         1 -0.531
    ## 2         2 -2.35
    ## 3         3  0.764
    ## 4         4  0.280
    ## 5         5  0.350

Please keep this in mind when writing infer code that utilizes
resampling with `generate()`.

## See also

Other core functions:
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md),
[`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md),
[`specify()`](https://infer.tidymodels.org/reference/specify.md)

## Examples

``` r
# generate a null distribution by taking 200 bootstrap samples
gss |>
 specify(response = hours) |>
 hypothesize(null = "point", mu = 40) |>
 generate(reps = 200, type = "bootstrap")
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 100,000 × 2
#> # Groups:   replicate [200]
#>    replicate hours
#>        <int> <dbl>
#>  1         1 38.6 
#>  2         1  8.62
#>  3         1 38.6 
#>  4         1 38.6 
#>  5         1 18.6 
#>  6         1 38.6 
#>  7         1 38.6 
#>  8         1 58.6 
#>  9         1 14.6 
#> 10         1 38.6 
#> # ℹ 99,990 more rows

# generate a null distribution for the independence of
# two variables by permuting their values 200 times
gss |>
 specify(partyid ~ age) |>
 hypothesize(null = "independence") |>
 generate(reps = 200, type = "permute")
#> Dropping unused factor levels DK from the supplied response variable
#> 'partyid'.
#> Response: partyid (factor)
#> Explanatory: age (numeric)
#> Null Hypothesi...
#> # A tibble: 100,000 × 3
#> # Groups:   replicate [200]
#>    partyid   age replicate
#>    <fct>   <dbl>     <int>
#>  1 dem        36         1
#>  2 dem        34         1
#>  3 ind        24         1
#>  4 dem        42         1
#>  5 ind        31         1
#>  6 rep        32         1
#>  7 ind        48         1
#>  8 ind        36         1
#>  9 ind        30         1
#> 10 rep        33         1
#> # ℹ 99,990 more rows

# generate a null distribution via sampling from a
# binomial distribution 200 times
gss |>
specify(response = sex, success = "female") |>
  hypothesize(null = "point", p = .5) |>
  generate(reps = 200, type = "draw") |>
  calculate(stat = "z")
#> Response: sex (factor)
#> Null Hypothesis: point
#> # A tibble: 200 × 2
#>    replicate    stat
#>        <int>   <dbl>
#>  1         1  0.537 
#>  2         2  0.447 
#>  3         3 -0.447 
#>  4         4 -0.984 
#>  5         5  1.70  
#>  6         6  1.52  
#>  7         7  0.0894
#>  8         8 -1.25  
#>  9         9 -0.268 
#> 10        10 -0.805 
#> # ℹ 190 more rows

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
