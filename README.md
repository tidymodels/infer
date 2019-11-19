
# infer R Package <img src="https://github.com/tidymodels/infer/blob/develop/figs/infer_gnome.png?raw=true" align="right" width=125 />

-----

<!--figs/infer.svg-->

<!--http://www.r-pkg.org/badges/version/infer-->

<!--figs/master.svg-->

<!--https://img.shields.io/codecov/c/github/tidymodels/infer/master.svg-->

[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/infer)](https://cran.r-project.org/package=infer)
[![Travis-CI Build
Status](https://travis-ci.org/tidymodels/infer.svg?branch=master)](https://travis-ci.org/tidymodels/infer)
[![AppVeyor Build
Status](https://ci.appveyor.com/api/projects/status/github/tidymodels/infer?branch=master&svg=true)](https://ci.appveyor.com/project/tidymodels/infer)
[![Coverage
Status](https://img.shields.io/codecov/c/github/tidymodels/infer/master.svg)](https://codecov.io/github/tidymodels/infer/?branch=master)

The objective of this package is to perform statistical inference using
an expressive statistical grammar that coheres with the `tidyverse`
design
framework.

![](https://raw.githubusercontent.com/tidymodels/infer/master/figs/ht-diagram.png)<!-- -->

### Installation

-----

To install the current stable version of `infer` from CRAN:

``` r
install.packages("infer")
```

To install the developmental version of `infer`, make sure to install
`remotes` first. The `pkgdown` website for this developmental version is
at <https://infer.netlify.com>.

``` r
install.packages("remotes")
remotes::install_github("tidymodels/infer")
```

To install the cutting edge version of `infer` (do so at your own risk),
make sure to install `remotes` first.

``` r
install.packages("remotes")
remotes::install_github("tidymodels/infer", ref = "develop")
```

To see the things we are working on with the package as
vignettes/Articles, check out the developmental `pkgdown` site at
<https://infer-dev.netlify.com>.

### Contributing

-----

We welcome others helping us make this package as user friendly and
efficient as possible. Please review our
[contributing](https://github.com/tidymodels/infer/blob/develop/CONDUCT.md)
and [conduct](CONDUCT.md) guidelines. Of particular interest is helping
us to write `testthat` tests and in building vignettes that show how to
(and how NOT to) use the package. By participating in this project you
agree to abide by its terms.

### Examples

-----

These examples assume that `mtcars` has been overwritten so that the
variables `cyl`, `vs`, `am`, `gear`, and `carb` are `factor`s.

``` r
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
          vs = factor(vs),
          am = factor(am),
          gear = factor(gear),
          carb = factor(carb))
```

Hypothesis test for a difference in proportions (using the formula
interface `y ~ x` in `specify()`):

``` r
mtcars %>%
  specify(am ~ vs, success = "1") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props", order = c("1", "0"))
```

Confidence interval for a difference in means (using the non-formula
interface giving both the `response` and `explanatory` variables in
`specify()`):

``` r
mtcars %>%
  specify(response = mpg, explanatory = am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))
```

Note that the formula and non-formula interfaces work for all
implemented inference procedures in `infer`. Use whatever is more
natural for you. If you will be doing modeling using functions like
`lm()` and `glm()`, we recommend you begin to use the formula `y ~ x`
notation as soon as possible though.

Other examples are available in the package vignettes.
