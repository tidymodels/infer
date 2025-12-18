# Define a theoretical distribution

This function allows the user to define a null distribution based on
theoretical methods. In many infer pipelines, `assume()` can be used in
place of
[`generate()`](https://infer.tidymodels.org/reference/generate.md) and
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md) to
create a null distribution. Rather than outputting a data frame
containing a distribution of test statistics calculated from resamples
of the observed data, `assume()` outputs a more abstract type of object
just containing the distributional details supplied in the
`distribution` and `df` arguments. However, `assume()` output can be
passed to
[`visualize()`](https://infer.tidymodels.org/reference/visualize.md),
[`get_p_value()`](https://infer.tidymodels.org/reference/get_p_value.md),
and
[`get_confidence_interval()`](https://infer.tidymodels.org/reference/get_confidence_interval.md)
in the same way that simulation-based distributions can.

To define a theoretical null distribution (for use in hypothesis
testing), be sure to provide a null hypothesis via
[`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md).
To define a theoretical sampling distribution (for use in confidence
intervals), provide the output of
[`specify()`](https://infer.tidymodels.org/reference/specify.md).
Sampling distributions (only implemented for `t` and `z`) lie on the
scale of the data, and will be recentered and rescaled to match the
corresponding `stat` given in
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md) to
calculate the observed statistic.

## Usage

``` r
assume(x, distribution, df = NULL, ...)
```

## Arguments

- x:

  The output of
  [`specify()`](https://infer.tidymodels.org/reference/specify.md) or
  [`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md),
  giving the observed data, variable(s) of interest, and (optionally)
  null hypothesis.

- distribution:

  The distribution in question, as a string. One of `"F"`, `"Chisq"`,
  `"t"`, or `"z"`.

- df:

  Optional. The degrees of freedom parameter(s) for the `distribution`
  supplied, as a numeric vector. For `distribution = "F"`, this should
  have length two (e.g. `c(10, 3)`). For `distribution = "Chisq"` or
  `distribution = "t"`, this should have length one. For
  `distribution = "z"`, this argument is not required. The package will
  supply a message if the supplied `df` argument is different from
  recognized values. See the Details section below for more information.

- ...:

  Currently ignored.

## Value

An infer theoretical distribution that can be passed to helpers like
[`visualize()`](https://infer.tidymodels.org/reference/visualize.md),
[`get_p_value()`](https://infer.tidymodels.org/reference/get_p_value.md),
and
[`get_confidence_interval()`](https://infer.tidymodels.org/reference/get_confidence_interval.md).

## Details

Note that the assumption being expressed here, for use in theory-based
inference, only extends to *distributional* assumptions: the null
distribution in question and its parameters. Statistical inference with
infer, whether carried out via simulation (i.e. based on pipelines using
[`generate()`](https://infer.tidymodels.org/reference/generate.md) and
[`calculate()`](https://infer.tidymodels.org/reference/calculate.md)) or
theory (i.e. with `assume()`), always involves the condition that
observations are independent of each other.

`infer` only supports theoretical tests on one or two means via the `t`
distribution and one or two proportions via the `z`.

For tests comparing two means, if `n1` is the group size for one level
of the explanatory variable, and `n2` is that for the other level,
`infer` will recognize the following degrees of freedom (`df`)
arguments:

- `min(n1 - 1, n2 - 1)`

- `n1 + n2 - 2`

- The `"parameter"` entry of the analogous
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) call

- The `"parameter"` entry of the analogous
  [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) call with
  `var.equal = TRUE`

By default, the package will use the `"parameter"` entry of the
analogous [`stats::t.test()`](https://rdrr.io/r/stats/t.test.html) call
with `var.equal = FALSE` (the default).

## Examples

``` r
# construct theoretical distributions ---------------------------------

# F distribution
# with the `partyid` explanatory variable
gss |>
  specify(age ~ partyid) |>
  assume(distribution = "F")
#> Dropping unused factor levels DK from the supplied explanatory
#> variable 'partyid'.
#> An F distribution with 3 and 496 degrees of freedom.

# Chi-squared goodness of fit distribution
# on the `finrela` variable
gss |>
  specify(response = finrela) |>
  hypothesize(null = "point",
              p = c("far below average" = 1/6,
                    "below average" = 1/6,
                    "average" = 1/6,
                    "above average" = 1/6,
                    "far above average" = 1/6,
                    "DK" = 1/6)) |>
  assume("Chisq")
#> A Chi-squared distribution with 5 degrees of freedom.

# Chi-squared test of independence
# on the `finrela` and `sex` variables
gss |>
  specify(formula = finrela ~ sex) |>
  assume(distribution = "Chisq")
#> A Chi-squared distribution with 5 degrees of freedom.

# T distribution
gss |>
  specify(age ~ college) |>
  assume("t")
#> A T distribution with 423 degrees of freedom.

# Z distribution
gss |>
  specify(response = sex, success = "female") |>
  assume("z")
#> A Z distribution.

if (FALSE) { # \dontrun{
# each of these distributions can be passed to infer helper
# functions alongside observed statistics!

# for example, a 1-sample t-test -------------------------------------

# calculate the observed statistic
obs_stat <- gss |>
  specify(response = hours) |>
  hypothesize(null = "point", mu = 40) |>
  calculate(stat = "t")

# construct a null distribution
null_dist <- gss |>
  specify(response = hours) |>
  assume("t")

# juxtapose them visually
visualize(null_dist) +
  shade_p_value(obs_stat, direction = "both")

# calculate a p-value
get_p_value(null_dist, obs_stat, direction = "both")

# or, an F test ------------------------------------------------------

# calculate the observed statistic
obs_stat <- gss |>
  specify(age ~ partyid) |>
  hypothesize(null = "independence") |>
  calculate(stat = "F")

# construct a null distribution
null_dist <- gss |>
  specify(age ~ partyid) |>
  assume(distribution = "F")

# juxtapose them visually
visualize(null_dist) +
  shade_p_value(obs_stat, direction = "both")

# calculate a p-value
get_p_value(null_dist, obs_stat, direction = "both")
} # }
```
