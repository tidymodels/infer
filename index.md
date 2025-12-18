# infer R Package

[![R-CMD-check](https://github.com/tidymodels/infer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/infer/actions/workflows/R-CMD-check.yaml)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/infer)](https://cran.r-project.org/package=infer)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/infer/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/infer)

The objective of this package is to perform statistical inference using
an expressive statistical grammar that coheres with the tidyverse design
framework. The package is centered around 4 main verbs, supplemented
with many utilities to visualize and extract value from their outputs.

- [`specify()`](https://infer.tidymodels.org/reference/specify.md)
  allows you to specify the variable, or relationship between variables,
  that you’re interested in.
- [`hypothesize()`](https://infer.tidymodels.org/reference/hypothesize.md)
  allows you to declare the null hypothesis.
- [`generate()`](https://infer.tidymodels.org/reference/generate.md)
  allows you to generate data reflecting the null hypothesis.
- [`calculate()`](https://infer.tidymodels.org/reference/calculate.md)
  allows you to calculate a distribution of statistics from the
  generated data to form the null distribution.

To learn more about the principles underlying the package design, see
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md).

![A diagram showing four steps to carry out randomization-based
inference: specify hypothesis, generate data, calculate statistic, and
visualize. From left to right, each step is connected by an arrow, while
the diagram indicates that generating data and calculating statistics
can happen
iteratively.](https://raw.githubusercontent.com/tidymodels/infer/main/figs/ht-diagram.png)

If you’re interested in learning more about randomization-based
statistical inference generally, including applied examples of this
package, we recommend checking out [Statistical Inference Via Data
Science: A ModernDive Into R and the
Tidyverse](https://moderndive.com/v2/) and [Introduction to Modern
Statistics](https://openintro-ims.netlify.app/).

### Installation

------------------------------------------------------------------------

To install the current stable version of infer from CRAN:

``` r
install.packages("infer")
```

To install the developmental stable version of infer, make sure to
install remotes first. The pkgdown website for this version is at
[infer.tidymodels.org](https://infer.tidymodels.org/).

``` r
# install.packages("pak")
pak::pak("tidymodels/infer")
```

### Contributing

------------------------------------------------------------------------

We welcome others helping us make this package as user-friendly and
efficient as possible. Please review our
[contributing](https://github.com/tidymodels/infer/blob/main/CONTRIBUTING.md)
and
[conduct](https://github.com/tidymodels/infer/blob/main/.github/CODE_OF_CONDUCT.md)
guidelines. By participating in this project you agree to abide by its
terms.

For questions and discussions about tidymodels packages, modeling, and
machine learning, please [post on Posit
Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).
If you think you have encountered a bug, please [submit an
issue](https://github.com/tidymodels/infer/issues). Either way, learn
how to create and share a
[reprex](https://reprex.tidyverse.org/articles/learn-reprex.html) (a
minimal, reproducible example), to clearly communicate about your code.
Check out further details on [contributing guidelines for tidymodels
packages](https://www.tidymodels.org/contribute/) and [how to get
help](https://www.tidymodels.org/help/).

### Examples

------------------------------------------------------------------------

These examples are pulled from the “Full infer Pipeline Examples”
vignette, accessible by calling
[`vignette("observed_stat_examples")`](https://infer.tidymodels.org/articles/observed_stat_examples.md).
They make use of the `gss` dataset supplied by the package, providing a
sample of data from the [General Social Survey](https://gss.norc.org).
The data looks like this:

``` r
# load in the dataset
data(gss)

# take a glimpse at it
str(gss)
```

``` R
## tibble [500 × 11] (S3: tbl_df/tbl/data.frame)
##  $ year   : num [1:500] 2014 1994 1998 1996 1994 ...
##  $ age    : num [1:500] 36 34 24 42 31 32 48 36 30 33 ...
##  $ sex    : Factor w/ 2 levels "male","female": 1 2 1 1 1 2 2 2 2 2 ...
##  $ college: Factor w/ 2 levels "no degree","degree": 2 1 2 1 2 1 1 2 2 1 ...
##  $ partyid: Factor w/ 5 levels "dem","ind","rep",..: 2 3 2 2 3 3 1 2 3 1 ...
##  $ hompop : num [1:500] 3 4 1 4 2 4 2 1 5 2 ...
##  $ hours  : num [1:500] 50 31 40 40 40 53 32 20 40 40 ...
##  $ income : Ord.factor w/ 12 levels "lt $1000"<"$1000 to 2999"<..: 12 11 12 12 12 12 12 12 12 10 ...
##  $ class  : Factor w/ 6 levels "lower class",..: 3 2 2 2 3 3 2 3 3 2 ...
##  $ finrela: Factor w/ 6 levels "far below average",..: 2 2 2 4 4 3 2 4 3 1 ...
##  $ weight : num [1:500] 0.896 1.083 0.55 1.086 1.083 ...
```

As an example, we’ll run an analysis of variance on `age` and `partyid`,
testing whether the age of a respondent is independent of their
political party affiliation.

Calculating the observed statistic,

``` r
F_hat <- gss |> 
  specify(age ~ partyid) |>
  calculate(stat = "F")
```

Then, generating the null distribution,

``` r
null_dist <- gss |>
   specify(age ~ partyid) |>
   hypothesize(null = "independence") |>
   generate(reps = 1000, type = "permute") |>
   calculate(stat = "F")
```

Visualizing the observed statistic alongside the null distribution,

``` r
visualize(null_dist) +
  shade_p_value(obs_stat = F_hat, direction = "greater")
```

![A histogram showing a distribution of F statistics, right-tailed and
centered around one. The x axis ranges from zero to five. The region of
the histogram to the right of the observed statistic, just above two, is
shaded red to represent the
p-value.](https://raw.githubusercontent.com/tidymodels/infer/main/README_files/figure-gfm/viz-1.png)

Calculating the p-value from the null distribution and observed
statistic,

``` r
null_dist |>
  get_p_value(obs_stat = F_hat, direction = "greater")
```

``` R
## # A tibble: 1 × 1
##   p_value
##     <dbl>
## 1   0.053
```

Note that the formula and non-formula interfaces (i.e., `age ~ partyid`
vs. `response = age, explanatory = partyid`) work for all implemented
inference procedures in `infer`. Use whatever is more natural for you.
If you will be doing modeling using functions like
[`lm()`](https://rdrr.io/r/stats/lm.html) and
[`glm()`](https://rdrr.io/r/stats/glm.html), though, we recommend you
begin to use the formula `y ~ x` notation as soon as possible.

Other resources are available in the package vignettes! See
[`vignette("observed_stat_examples")`](https://infer.tidymodels.org/articles/observed_stat_examples.md)
for more examples like the one above, and
[`vignette("infer")`](https://infer.tidymodels.org/articles/infer.md)
for discussion of the underlying principles of the package design.
