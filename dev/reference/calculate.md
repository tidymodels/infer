# Calculate summary statistics

Given the output of
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md)
and/or
[`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md),
this function will return the observed statistic specified with the
`stat` argument. Some test statistics, such as `Chisq`, `t`, and `z`,
require a null hypothesis. If provided the output of
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md),
the function will calculate the supplied `stat` for each `replicate`.

Learn more in
[`vignette("infer")`](https://infer.tidymodels.org/dev/articles/infer.md).

## Usage

``` r
calculate(
  x,
  stat = c("mean", "median", "sum", "sd", "prop", "count", "diff in means",
    "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z",
    "ratio of props", "odds ratio", "ratio of means"),
  order = NULL,
  ...
)
```

## Arguments

- x:

  The output from
  [`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)
  for computation-based inference or the output from
  [`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md)
  piped in to here for theory-based inference.

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

A tibble containing a `stat` column of calculated statistics.

## Arbitrary test statistics

In addition to the pre-implemented statistics documented in `stat`,
users can supply an arbitrary test statistic by supplying a function to
the `stat` argument.

The function should have arguments `stat(x, order, ...)`, where `x` is
one replicate's worth of `x`. The `order` argument and ellipses will be
supplied directly to the `stat` function. Internally, `calculate()` will
split `x` up into data frames by replicate and pass them one-by-one to
the supplied `stat`. For example, to implement `stat = "mean"` as a
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
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)d
and
non-[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md)d
infer objects–no need to implement support for grouping by `replicate`
yourself.

## Missing levels in small samples

In some cases, when bootstrapping with small samples, some generated
bootstrap samples will have only one level of the explanatory variable
present. For some test statistics, the calculated statistic in these
cases will be NaN. The package will omit non-finite values from
visualizations (with a warning) and raise an error in p-value
calculations.

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

## See also

[`visualize()`](https://infer.tidymodels.org/dev/reference/visualize.md),
[`get_p_value()`](https://infer.tidymodels.org/dev/reference/get_p_value.md),
and
[`get_confidence_interval()`](https://infer.tidymodels.org/dev/reference/get_confidence_interval.md)
to extract value from this function's outputs.

Other core functions:
[`generate()`](https://infer.tidymodels.org/dev/reference/generate.md),
[`hypothesize()`](https://infer.tidymodels.org/dev/reference/hypothesize.md),
[`specify()`](https://infer.tidymodels.org/dev/reference/specify.md)

## Examples

``` r
# calculate a null distribution of hours worked per week under
# the null hypothesis that the mean is 40
gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  generate(reps = 200, type = "bootstrap") |>
  calculate(stat = "mean")
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 200 × 2
#>    replicate  stat
#>        <int> <dbl>
#>  1         1  39.0
#>  2         2  39.5
#>  3         3  40.1
#>  4         4  39.6
#>  5         5  40.7
#>  6         6  40.0
#>  7         7  39.9
#>  8         8  40.8
#>  9         9  39.5
#> 10        10  40.9
#> # ℹ 190 more rows

# calculate the corresponding observed statistic
gss |>
  specify(response = hours) |>
  calculate(stat = "mean")
#> Response: hours (numeric)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1  41.4

# calculate a null distribution assuming independence between age
# of respondent and whether they have a college degree
gss |>
  specify(age ~ college) |>
  hypothesize(null = "independence") |>
  generate(reps = 200, type = "permute") |>
  calculate("diff in means", order = c("degree", "no degree"))
#> Response: age (numeric)
#> Explanatory: college (factor)
#> Null Hypothesi...
#> # A tibble: 200 × 2
#>    replicate     stat
#>        <int>    <dbl>
#>  1         1 -1.65   
#>  2         2  1.19   
#>  3         3 -0.0113 
#>  4         4  0.579  
#>  5         5  0.553  
#>  6         6  1.84   
#>  7         7 -2.31   
#>  8         8 -0.320  
#>  9         9 -0.00250
#> 10        10 -1.78   
#> # ℹ 190 more rows

# calculate the corresponding observed statistic
gss |>
  specify(age ~ college) |>
  calculate("diff in means", order = c("degree", "no degree"))
#> Response: age (numeric)
#> Explanatory: college (factor)
#> # A tibble: 1 × 1
#>    stat
#>   <dbl>
#> 1 0.941

# some statistics require a null hypothesis
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

# more in-depth explanation of how to use the infer package
if (FALSE) { # \dontrun{
vignette("infer")
} # }
```
