
Infer
=====

<table>
<colgroup>
<col width="100%" />
</colgroup>
<tbody>
<tr class="odd">
<td><a href="http://cran.rstudio.com/web/packages/infer/index.html"><img src="http://cranlogs.r-pkg.org/badges/infer" /></a> <a href="https://travis-ci.org/andrewpbray/infer"><img src="https://travis-ci.org/andrewpbray/infer.svg?branch=master" alt="Travis-CI Build Status" /></a> <a href="https://codecov.io/github/andrewpbray/infer/?branch=master"><img src="https://img.shields.io/codecov/c/github/andrewpbray/infer//master.svg" alt="Coverage Status" /></a></td>
</tr>
<tr class="even">
<td>The objective of this package is to perform statistical inference using an expressive statistical grammar that coheres with the <code>tidyverse</code> design framework.</td>
</tr>
<tr class="odd">
<td><img src="https://raw.githubusercontent.com/andrewpbray/infer/master/figs/ht-diagram.png" /></td>
</tr>
<tr class="even">
<td>### Installation</td>
</tr>
</tbody>
</table>

To install the current stable version of `infer` from CRAN:

``` r
install.packages("infer")
```

To install the developmental version of `infer`, make sure to install `remotes` first:

``` r
install.packages("remotes")
remotes::install_github("andrewpbray/infer")
```

### Examples

------------------------------------------------------------------------

These examples assume that `mtcars` has been overwritten so that the variables `cyl`, `vs`, `am`, `gear`, and `carb` are `factor`s.

``` r
mtcars <- as.data.frame(mtcars) %>%
  mutate(cyl = factor(cyl),
          vs = factor(vs),
          am = factor(am),
          gear = factor(gear),
          carb = factor(carb))
```

Hypothesis test for a difference in proportions:

``` r
mtcars %>%
  specify(am ~ vs, success = "1") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  calculate(stat = "diff in props", order = c("1", "0"))
```

Confidence interval for a difference in means:

``` r
mtcars %>%
  specify(mpg ~ am) %>%
  generate(reps = 100, type = "bootstrap") %>%
  calculate(stat = "diff in means", order = c("1", "0"))
```
