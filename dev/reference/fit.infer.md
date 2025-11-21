# Fit linear models to infer objects

Given the output of an infer core function, this function will fit a
linear model using [`stats::glm()`](https://rdrr.io/r/stats/glm.html)
according to the formula and data supplied earlier in the pipeline. If
passed the output of
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md) or
[`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md),
the function will fit one model. If passed the output of
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md),
it will fit a model to each data resample, denoted in the `replicate`
column. The family of the fitted model depends on the type of the
response variable. If the response is numeric,
[`fit()`](https://generics.r-lib.org/reference/fit.html) will use
`family = "gaussian"` (linear regression). If the response is a 2-level
factor or character,
[`fit()`](https://generics.r-lib.org/reference/fit.html) will use
`family = "binomial"` (logistic regression). To fit character or factor
response variables with more than two levels, we recommend
[`parsnip::multinom_reg()`](https://parsnip.tidymodels.org/reference/multinom_reg.html).

infer provides a fit "method" for infer objects, which is a way of
carrying out model fitting as applied to infer output. The "generic,"
imported from the generics package and re-exported from this package,
provides the general form of
[`fit()`](https://generics.r-lib.org/reference/fit.html) that points to
infer's method when called on an infer object. That generic is also
documented here.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/dev/articles/infer.md).

## Usage

``` r
# S3 method for class 'infer'
fit(object, ...)
```

## Arguments

- object:

  Output from an infer function—likely
  [`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)
  or
  [`specify()`](https://infer.tidymodels.org/dev/reference/specify.md)—which
  specifies the formula and data to fit a model to.

- ...:

  Any optional arguments to pass along to the model fitting function.
  See [`stats::glm()`](https://rdrr.io/r/stats/glm.html) for more
  information.

## Value

A [tibble](https://tibble.tidyverse.org/reference/tibble.html)
containing the following columns:

- `replicate`: Only supplied if the input object had been previously
  passed to
  [`generate()`](https://infer.tidymodels.org/dev/reference/generate.md).
  A number corresponding to which resample of the original data set the
  model was fitted to.

- `term`: The explanatory variable (or intercept) in question.

- `estimate`: The model coefficient for the given resample (`replicate`)
  and explanatory variable (`term`).

## Details

Randomization-based statistical inference with multiple explanatory
variables requires careful consideration of the null hypothesis in
question and its implications for permutation procedures. Inference for
partial regression coefficients via the permutation method implemented
in
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)
for multiple explanatory variables, consistent with its meaning
elsewhere in the package, is subject to additional distributional
assumptions beyond those required for one explanatory variable. Namely,
the distribution of the response variable must be similar to the
distribution of the errors under the null hypothesis' specification of a
fixed effect of the explanatory variables. (This null hypothesis is
reflected in the `variables` argument to
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md).
By default, all of the explanatory variables are treated as fixed.) A
general rule of thumb here is, if there are large outliers in the
distributions of any of the explanatory variables, this distributional
assumption will not be satisfied; when the response variable is
permuted, the (presumably outlying) value of the response will no longer
be paired with the outlier in the explanatory variable, causing an
outsize effect on the resulting slope coefficient for that explanatory
variable.

More sophisticated methods that are outside of the scope of this package
requiring fewer—or less strict—distributional assumptions exist. For an
overview, see "Permutation tests for univariate or multivariate analysis
of variance and regression" (Marti J. Anderson, 2001),
[doi:10.1139/cjfas-58-3-626](https://doi.org/10.1139/cjfas-58-3-626) .

## Reproducibility

When using the infer package for research, or in other cases when exact
reproducibility is a priority, be sure the set the seed for R’s random
number generator. infer will respect the random seed specified in the
[`set.seed()`](https://rdrr.io/r/base/Random.html) function, returning
the same result when
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)ing
data given an identical seed. For instance, we can calculate the
difference in mean `age` by `college` degree status using the `gss`
dataset from 10 versions of the `gss` resampled with permutation using
the following code.

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
resampling with
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md).

## Examples

``` r
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
#>  1         1 intercept      39.8   
#>  2         1 age             0.0479
#>  3         1 collegedegree  -1.01  
#>  4         2 intercept      42.6   
#>  5         2 age            -0.0276
#>  6         2 collegedegree  -0.304 
#>  7         3 intercept      39.6   
#>  8         3 age             0.0552
#>  9         3 collegedegree  -1.17  
#> 10         4 intercept      43.1   
#> # ℹ 290 more rows

# for logistic regression, just supply a binary response variable!
# (this can also be made explicit via the `family` argument in ...)
gss |>
  specify(college ~ age + hours) |>
  fit()
#> # A tibble: 3 × 2
#>   term      estimate
#>   <chr>        <dbl>
#> 1 intercept -1.13   
#> 2 age        0.00527
#> 3 hours      0.00698

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
