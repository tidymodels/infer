# infer (development version)

* Introduced support for arbitrary test statistics in `calculate()`. In addition
  to the pre-implemented `calculate(stat)` options, taken as strings, users can
  now supply a function defining any scalar-valued test statistic. See
  `?calculate()` to learn more.

# infer 1.0.9

* Replaced usage of deprecated functions ahead of a new release of the ggplot2 package (#557).

* Addressed narrative mistakes in the `t_test` vignette (#556).

* Increased the minimum required R version to R 4.1

# infer 1.0.8

* The infer print method now truncates output when descriptions of explanatory or responses variables exceed the console width (#543).

* Added missing commas and addressed formatting issues throughout the vignettes and articles. Backticks for package names were removed and missing parentheses for functions were added (@Joscelinrocha).


# infer 1.0.7

* The aliases `p_value()` and `conf_int()`, first deprecated 6 years ago, now
  return an error (#530).
  
* Addresses ggplot2 warnings when shading p-values for test statistics
  that are outside of the range of the generated distribution (#528).

* Fixed bug in `shade_p_value()` and `shade_confidence_interval()` where `fill = NULL` was ignored when it was documented as preventing any shading (#525).

# infer v1.0.6

* Updated infrastructure for errors, warnings, and messages (#513). Most of these changes will not be visible to users, though:
     - Many longer error messages are now broken up into several lines.
     - For references to help-files, users can now click on the error message's text to navigate to the cited documentation.
     
* Various improvements to documentation (#501, #504, #508, #512).

* Fixed bug where `get_confidence_interval()` would error uninformatively when the supplied distribution of estimates contained missing values. The function will now warn and return a confidence interval calculated using the non-missing estimates (#521).

* Fixed bug where `generate()` could not be used without first `specify()`ing variables, even in cases where that specification would not affect resampling/simulation (#448). 

# infer v1.0.5

* Implemented support for permutation hypothesis tests for paired data via the 
  argument value `null = "paired independence"` in `hypothesize()` (#487).

* The `weight_by` argument to `rep_slice_sample()` can now be passed either as a vector of numeric weights or an unquoted column name in `.data` (#480).

* Newly accommodates variables with spaces in names in the wrapper functions `t_test()` and `prop_test()` (#472).

* Fixed bug in two-sample `prop_test()` where the response and explanatory 
  variable were passed in place of each other to `prop.test()`. This enables
  using `prop_test()` with explanatory variables with greater than 2 levels and,
  in the process, addresses a bug where `prop_test()` collapsed levels other than
  the `success` when the response variable had more than 2 levels.

# infer v1.0.4

* Fixed bug in p-value shading where shaded regions no longer correctly overlaid
  histogram bars.
* Addressed deprecation warning ahead of upcoming dplyr release.

# infer v1.0.3

* Fix R-devel HTML5 NOTEs.

# infer v1.0.2

* Fix p-value shading when the calculated statistic falls exactly on the boundaries of a histogram bin (#424).
* Fix `generate()` errors when columns are named `x` (#431).
* Fix error from `visualize` when passed `generate()`d `infer_dist` objects that had not been passed to `hypothesize()` (#432). 
* Update visual checks for `visualize` output to align with the R 4.1.0+ graphics engine (#438).
* `specify()` and wrapper functions now appropriately handle ordered factors (#439).
* Clarify error when incompatible statistics and hypotheses are supplied (#441).
* Updated `generate()` unexpected `type` warnings to be more permissive—the 
warning will be raised less often when `type = "bootstrap"` (#425).
* Allow passing additional arguments to `stats::chisq.test` via `...` in 
`calculate()`. Ellipses are now always passed to the applicable base R
hypothesis testing function, when applicable (#414)!
* The package will now set the levels of logical variables on conversion to factor
so that the first level (regarded as `success` by default) is `TRUE`. Core verbs
have warned without an explicit `success` value already, and this change makes
behavior consistent with the functions being wrapped by shorthand test 
wrappers (#440).
* Added new statistic `stat = "ratio of means"` (#452).

# infer v1.0.1 (GitHub Only)

This release reflects the infer version accepted to the Journal of Open Source Software.

* Re-licensed the package from CC0 to MIT. See the `LICENSE` and `LICENSE.md` files.
* Contributed a paper to the Journal of Open Source Software, a draft of which is available in `/figs/paper`.
* Various improvements to documentation (#417, #418).

# infer 1.0.0

v1.0.0 is the first major release of the {infer} package! By and large, the core verbs `specify()`, `hypothesize()`, `generate()`, and `calculate()` will interface as they did before. This release makes several improvements to behavioral consistency of the package and introduces support for theory-based inference as well as randomization-based inference with multiple explanatory variables.

## Behavioral consistency

A major change to the package in this release is a set of standards for behavioral consistency of `calculate()` (#356). Namely, the package will now

* supply a consistent error when the supplied `stat` argument isn't well-defined
for the variables `specify()`d

``` r
gss %>%
  specify(response = hours) %>%
  calculate(stat = "diff in means")
#> Error: A difference in means is not well-defined for a 
#> numeric response variable (hours) and no explanatory variable.
```

or

``` r
gss %>%
  specify(college ~ partyid, success = "degree") %>%
  calculate(stat = "diff in props")
#> Error: A difference in proportions is not well-defined for a dichotomous categorical 
#> response variable (college) and a multinomial categorical explanatory variable (partyid).
```

* supply a consistent message when the user supplies unneeded information via `hypothesize()` to `calculate()` an observed statistic

``` r
# supply mu = 40 when it's not needed
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "mean")
#> Message: The point null hypothesis `mu = 40` does not inform calculation of 
#> the observed statistic (a mean) and will be ignored.
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  41.4
```

and

* supply a consistent warning and assume a reasonable null value when the user does not supply sufficient information to calculate an observed statistic

``` r
# don't hypothesize `p` when it's needed
gss %>%
    specify(response = sex, success = "female") %>%
    calculate(stat = "z")
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1 -1.16
#> Warning message:
#> A z statistic requires a null hypothesis to calculate the observed statistic. 
#> Output assumes the following null value: `p = .5`. 
```

or

``` r
# don't hypothesize `p` when it's needed
gss %>%
  specify(response = partyid) %>%
  calculate(stat = "Chisq")
#> # A tibble: 1 x 1
#>    stat
#>  <dbl>
#> 1  334.
#> Warning message:
#> A chi-square statistic requires a null hypothesis to calculate the observed statistic. 
#> Output assumes the following null values: `p = c(dem = 0.2, ind = 0.2, rep = 0.2, other = 0.2, DK = 0.2)`.
```

To accommodate this behavior, a number of new `calculate` methods were added or improved. Namely:

- Implemented the standardized proportion $z$ statistic for one categorical variable
- Extended `calculate()` with `stat = "t"` by passing `mu` to the `calculate()` method for `stat = "t"` to allow for calculation of `t` statistics for one numeric variable with hypothesized mean
- Extended `calculate()` to allow lowercase aliases for `stat` arguments (#373).
- Fixed bugs in `calculate()` for to allow for programmatic calculation of statistics

This behavioral consistency also allowed for the implementation of `observe()`, a wrapper function around `specify()`, `hypothesize()`, and `calculate()`, to calculate observed statistics. The function provides a shorthand alternative to calculating observed statistics from data:

``` r
# calculating the observed mean number of hours worked per week
gss %>%
  observe(hours ~ NULL, stat = "mean")
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  41.4

# equivalently, calculating the same statistic with the core verbs
gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  41.4

# calculating a t statistic for hypothesized mu = 40 hours worked/week
gss %>%
  observe(hours ~ NULL, stat = "t", null = "point", mu = 40)
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  2.09

# equivalently, calculating the same statistic with the core verbs
gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  2.09
```

We don't anticipate that these changes are "breaking" in the sense that code that previously worked will continue to, though it may now message or warn in a way that it did not used to or error with a different (and hopefully more informative) message.

## A framework for theoretical inference

This release also introduces a more complete and principled interface for theoretical inference. While the package previously supplied some methods for visualization of theory-based curves, the interface did not provide any object that was explicitly a "null distribution" that could be supplied to helper functions like `get_p_value()` and `get_confidence_interval()`. The new interface is based on a new verb, `assume()`, that returns a null distribution that can be interfaced in the same way that simulation-based null distributions can be interfaced with.

As an example, we'll work through a full infer pipeline for inference on a mean using infer's `gss` dataset. Supposed that we believe the true mean number of hours worked by Americans in the past week is 40.

First, calculating the observed `t`-statistic:

``` r
obs_stat <- gss %>%
  specify(response = hours) %>%
  hypothesize(null = "point", mu = 40) %>%
  calculate(stat = "t")

obs_stat
#> Response: hours (numeric)
#> Null Hypothesis: point
#> # A tibble: 1 x 1
#>    stat
#>   <dbl>
#> 1  2.09
```

The code to define the null distribution is very similar to that required to calculate a theorized observed statistic, switching out `calculate()` for `assume()` and replacing arguments as needed.

``` r
null_dist <- gss %>%
  specify(response = hours) %>%
  assume(distribution = "t")

null_dist 
#> A T distribution with 499 degrees of freedom.
```

This null distribution can now be interfaced with in the same way as a simulation-based null distribution elsewhere in the package. For example, calculating a p-value by juxtaposing the observed statistic and null distribution:

``` r
get_p_value(null_dist, obs_stat, direction = "both")
#> # A tibble: 1 x 1
#>   p_value
#>     <dbl>
#> 1  0.0376
```

…or visualizing the null distribution alone:

``` r
visualize(null_dist)
```

![](https://i.imgur.com/g3B5coD.png)

…or juxtaposing the two visually:

``` r
visualize(null_dist) + 
  shade_p_value(obs_stat, direction = "both")
```

![](https://i.imgur.com/3C66kgK.png)

Confidence intervals lie in data space rather than the standardized scale of the theoretical distributions. Calculating a mean rather than the standardized `t`-statistic:

``` r
obs_mean <- gss %>%
  specify(response = hours) %>%
  calculate(stat = "mean")
```

The null distribution here just defines the spread for the standard error calculation.

``` r
ci <- 
  get_confidence_interval(
    null_dist,
    level = .95,
    point_estimate = obs_mean
  )

ci
#> # A tibble: 1 x 2
#>   lower_ci upper_ci
#>      <dbl>    <dbl>
#> 1     40.1     42.7
```

Visualizing the confidence interval results in the theoretical distribution being recentered and rescaled to align with the scale of the observed data:

``` r
visualize(null_dist) + 
  shade_confidence_interval(ci)
```

![](https://i.imgur.com/4akSCY3.png)

Previous methods for interfacing with theoretical distributions are superseded—they will continue to be supported, though documentation will forefront the `assume()` interface.

## Support for multiple regression

The 2016 "Guidelines for Assessment and Instruction in Statistics Education" [1] state that, in introductory statistics courses, "[s]tudents should gain experience with how statistical models, including multivariable models, are used." In line with this recommendation, we introduce support for randomization-based inference with multiple explanatory variables via a new `fit.infer` core verb.

If passed an `infer` object, the method will parse a formula out of the `formula` or `response` and `explanatory` arguments, and pass both it and `data` to a `stats::glm` call.

``` r
gss %>%
  specify(hours ~ age + college) %>%
  fit()
#> # A tibble: 3 x 2
#>   term          estimate
#>   <chr>            <dbl>
#> 1 intercept     40.6    
#> 2 age            0.00596
#> 3 collegedegree  1.53
```

Note that the function returns the model coefficients as `estimate` rather than their associated `t`-statistics as `stat`.

If passed a `generate()`d object, the model will be fitted to each replicate.

``` r
gss %>%
  specify(hours ~ age + college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute") %>%
  fit()
#> # A tibble: 300 x 3
#> # Groups:   replicate [100]
#>    replicate term          estimate
#>        <int> <chr>            <dbl>
#>  1         1 intercept     44.4    
#>  2         1 age           -0.0767 
#>  3         1 collegedegree  0.121  
#>  4         2 intercept     41.8    
#>  5         2 age            0.00344
#>  6         2 collegedegree -1.59   
#>  7         3 intercept     38.3    
#>  8         3 age            0.0761 
#>  9         3 collegedegree  0.136  
#> 10         4 intercept     43.1    
#> # … with 290 more rows
```

If `type = "permute"`, a set of unquoted column names in the data to permute (independently of each other) can be passed via the `variables` argument to `generate`. It defaults to only the response variable.

``` r
gss %>%
  specify(hours ~ age + college) %>%
  hypothesize(null = "independence") %>%
  generate(reps = 100, type = "permute", variables = c(age, college)) %>%
  fit()
#> # A tibble: 300 x 3
#> # Groups:   replicate [100]
#>    replicate term          estimate
#>        <int> <chr>            <dbl>
#>  1         1 intercept      39.4   
#>  2         1 age             0.0748
#>  3         1 collegedegree  -2.98  
#>  4         2 intercept      42.8   
#>  5         2 age            -0.0190
#>  6         2 collegedegree  -1.83  
#>  7         3 intercept      40.4   
#>  8         3 age             0.0354
#>  9         3 collegedegree  -1.31  
#> 10         4 intercept      40.9   
#> # … with 290 more rows
```

This feature allows for more detailed exploration of the effect of disrupting the correlation structure among explanatory variables on outputted model coefficients.

Each of the auxillary functions `get_p_value()`, `get_confidence_interval()`, `visualize()`, `shade_p_value()`, and `shade_confidence_interval()` have methods to handle `fit()` output! See their help-files for example usage. Note that `shade_*` functions now delay evaluation until they are added to an existing ggplot (e.g. that outputted by `visualize()`) with `+`.

## Improvements

- Following extensive discussion, the `generate()` type `type = "simulate"` has been renamed to the more evocative `type = "draw"`. We will continue to support `type = "simulate"` indefinitely, though supplying that argument will now prompt a message notifying the user of its preferred alias. (#233, #390)
- Fixed several bugs related to factors with unused levels. `specify()` will now drop unused factor levels and message that it has done so. (#374, #375, #397, #380)
- Added `two.sided` as an acceptable alias for `two_sided` for the `direction` argument in `get_p_value()` and `shade_p_value()`. (#355)
- Various improvements to documentation, including extending example sections in help-files, re-organizing the function reference in the {pkgdown} site, and linking more extensively among help-files.

## Breaking changes

We don't anticipate that any changes made in this release are "breaking" in the sense that code that previously worked will continue to, though it may now message or warn in a way that it did not used to or error with a different (and hopefully more informative) message. If you currently teach or research with infer, we recommend re-running your materials and noting any changes in messaging and warning.

- Move forward with a number of planned deprecations. Namely, the `GENERATION_TYPES` object is now fully deprecated, and arguments that were relocated from `visualize()` to `shade_p_value()` and `shade_confidence_interval()` are now fully deprecated in `visualize()`. If supplied a deprecated argument, `visualize()` will warn the user and ignore the argument.
- Added a `prop` argument to `rep_slice_sample()` as an alternative to the `n`
argument for specifying the proportion of rows in the supplied data to sample
per replicate (#361, #362, #363). This changes order of arguments of
`rep_slice_sample()` (in order to be more aligned with `dplyr::slice_sample()`)
which might break code if it didn't use named arguments (like
`rep_slice_sample(df, 5, TRUE)`). To fix this, use named arguments (like
`rep_slice_sample(df, 5, replicate = TRUE)`).

## Other

- Added Simon P. Couch as an author. Long deserved for his reliable maintenance and improvements of the package.

[1]: GAISE College Report ASA Revision Committee, "Guidelines for Assessment and Instruction in Statistics Education College Report 2016," http://www.amstat.org/education/gaise. 

# infer 0.5.4

- `rep_sample_n()` no longer errors when supplied a `prob` argument (#279)
- Added `rep_slice_sample()`, a light wrapper around `rep_sample_n()`, that
more closely resembles `dplyr::slice_sample()` (the function that supersedes
`dplyr::sample_n()`) (#325)
- Added a `success`, `correct`, and `z` argument to `prop_test()` 
(#343, #347, #353)
- Implemented observed statistic calculation for the standardized proportion 
$z$ statistic (#351, #353)
- Various bug fixes and improvements to documentation and errors.

# infer 0.5.3

## Breaking changes

- `get_confidence_interval()` now uses column names ('lower_ci' and 'upper_ci') 
in output that are consistent with other infer functionality (#317).

## New functionality

- `get_confidence_interval()` can now produce bias-corrected confidence intervals
by setting `type = "bias-corrected"`. Thanks to @davidbaniadam for the 
initial implementation (#237, #318)!

## Other

- Fix CRAN check failures related to long double errors.

# infer 0.5.2

- Warn the user when a p-value of 0 is reported (#257, #273)
- Added new vignettes: `chi_squared` and `anova` (#268)
- Updates to documentation and existing vignettes (#268)
- Add alias for `hypothesize()` (`hypothesise()`) (#271)
- Subtraction order no longer required for difference-based tests--a warning will be raised in the case that the user doesn't supply an `order` argument (#275, #281)
- Add new messages for common errors (#277)
- Increase coverage of theoretical methods in documentation (#278, #280)
- Drop missing values and reduce size of `gss` dataset used in examples (#282)
- Add `stat = "ratio of props"` and `stat = "odds ratio"` to `calculate` (#285)
- Add `prop_test()`, a tidy interface to `prop.test()` (#284, #287)
- Updates to `visualize()` for compatibility with `ggplot2` v3.3.0 (#289)
- Fix error when bootstrapping with small samples and raise warnings/errors 
when appropriate (#239, #244, #291)
- Fix unit test failures resulting from breaking changes in `dplyr` v1.0.0
- Fix error in `generate()` when response variable is named `x` (#299)
- Add `two-sided` and `two sided` as aliases for `two_sided` for the 
`direction` argument in `get_p_value()` and `shade_p_value()` (#302)
- Fix `t_test()` and `t_stat()` ignoring the `order` argument (#310)

# infer 0.5.1

- Updates to documentation and other tweaks

# infer 0.5.0

## Breaking changes

- `shade_confidence_interval()` now plots vertical lines starting from zero (previously - from the bottom of a plot) (#234).
- `shade_p_value()` now uses "area under the curve" approach to shading (#229).

## Other

- Updated `chisq_test()` to take arguments in a response/explanatory format, perform goodness of fit tests, and default to the approximation approach (#241).
- Updated `chisq_stat()` to do goodness of fit (#241).
- Make interface to `hypothesize()` clearer by adding the options for the point null parameters to the function signature (#242).
- Manage `infer` class more systematically (#219).
- Use `vdiffr` for plot testing (#221).

# infer 0.4.1

- Added Evgeni Chasnovski as author for his incredible work on refactoring the package and providing excellent support.

# infer 0.4.0

## Breaking changes

- Changed method of computing two-sided p-value to a more conventional one. It also makes `get_pvalue()` and `visualize()` more aligned (#205).

## Deprecation changes

- Deprecated `p_value()` (use `get_p_value()` instead) (#180).
- Deprecated `conf_int()` (use `get_confidence_interval()` instead) (#180).
- Deprecated (via warnings) plotting p-value and confidence interval in `visualize()` (use new functions `shade_p_value()` and `shade_confidence_interval()` instead) (#178).

## New functions

- `shade_p_value()` - {ggplot2}-like layer function to add information about p-value region to `visualize()` output. Has alias `shade_pvalue()`.
- `shade_confidence_interval()` - {ggplot2}-like layer function to add information about confidence interval region to `visualize()` output. Has alias `shade_ci()`.

## Other

- Account for `NULL` value in left hand side of formula in `specify()` (#156) and `type` in `generate()` (#157).
- Update documentation code to follow tidyverse style guide (#159).
- Remove help page for internal `set_params()` (#165).
- Fully use {tibble} (#166).
- Fix `calculate()` to not depend on order of `p` for `type = "simulate"` (#122).
- Reduce code duplication (#173).
- Make transparency in `visualize()` to not depend on method and data volume.
- Make `visualize()` work for "One sample t" theoretical type with `method = "both"`.
- Add `stat = "sum"` and `stat = "count"` options to `calculate()` (#50).

# infer 0.3.1

- Stop using package {assertive} in favor of custom type checks (#149)
- Fixed `t_stat()` to use `...` so `var.equal` works
- With the help of @echasnovski, fixed `var.equal = TRUE` for `specify() %>% calculate(stat = "t")`
- Use custom functions for error, warning, message, and `paste()` handling (#155)

# infer 0.3.0

- Added `conf_int` logical argument and `conf_level` argument to `t_test()`
- Switched `shade_color` argument in `visualize()` to be `pvalue_fill` instead
since fill color for confidence intervals is also added now
- Shading for Confidence Intervals in `visualize()` 
    - Green is default color for CI and red for p-values
    - `direction = "between"` to get the green shading
    - Currently working only for simulation-based methods
- Implemented `conf_int()` function for computing confidence interval provided a simulation-based method with a `stat` variable
    - `get_ci()` and `get_confidence_interval()` are aliases for `conf_int()`
    - Converted longer confidence interval calculation code in vignettes to use `get_ci()` instead    
- Implemented `p_value()` function for computing p-value provided a simulation-based method with a `stat` variable
    - `get_pvalue()` is an alias for `p_value()`
    - Converted longer p-value calculation code in vignettes to use `get_pvalue()` instead
- Implemented Chi-square Goodness of Fit observed stat depending on `params` being set in `hypothesize` with `specify() %>% calculate()` shortcut
- Removed "standardized" slope $t$ since its formula is different than "standardized" correlation and there is no way currently to give one over the other
- Implemented correlation with bootstrap CI and permutation hypothesis test
- Filled the `type` argument automatically in `generate()` based
on `specify()` and `hypothesize()`
    - Added message if `type` is given differently than expected
- Implemented `specify() %>% calculate()` for getting observed
statistics.
    - `visualize()` works with either a 1x1 data frame or a vector
    for its `obs_stat` argument
    - Got `stat = "t"` working
- Refactored `calculate()` into smaller functions to reduce complexity
- Produced error if `mu` is given in `hypothesize()` but `stat = "median"`
is provided in `calculate()` and other similar mis-specifications
- Tweaked `chisq_stat()` and `t_stat()` to match with `specify() %>% calculate()` framework
    - Both work in the one sample and two sample cases by providing `formula`
    - Added `order` argument to `t_stat()`
- Added implementation of one sample `t_test()` by passing in the `mu` argument to `t.test`
from `hypothesize()`
- Tweaked `pkgdown` page to include ToDo's using [{dplyr}](https://github.com/tidyverse/dplyr/blob/master/_pkgdown.yml) example

# infer 0.2.0

- Switched to `!!` instead of `UQ()` since `UQ()` is deprecated in 
{rlang} 0.2.0
- Added many new files: `CONDUCT.md`, `CONTRIBUTING.md`, and `TO-DO.md`
- Updated README file with more development information
- Added wrapper functions `t_test()` and `chisq_test()` that use a
formula interface and provide an intuitive wrapper to `t.test()` and
`chisq.test()`
- Created `stat = "z"` and `stat = "t"` options
- Added many new arguments to `visualize()` to prescribe colors to shade and 
use for observed statistics and theoretical density curves
- Added check so that a bar graph created with `visualize()` if number of 
unique values for generated statistics is small
- Added shading for `method = "theoretical"` 
- Implemented shading for simulation methods w/o a traditional distribution
    - Use percentiles to determine two-tailed shading
- Changed `method = "randomization"` to `method = "simulation"`
- Added warning when theoretical distribution is used that 
  assumptions should be checked  
- Added theoretical distributions to `visualize()` alone and as overlay with
current implementations being
    - Two sample t
    - ANOVA F
    - One proportion z
    - Two proportion z
    - Chi-square test of independence
    - Chi-square Goodness of Fit test
    - Standardized slope (t)
    
# infer 0.1.1
- Added additional tests
- Added `order` argument in `calculate()`
- Fixed bugs post-CRAN release
- Automated travis build of pkgdown to gh-pages branch

# infer 0.1.0
- Altered the way that successes are indicated in an infer pipeline. 
They now live in `specify()`.
- Updated documentation with examples
- Created `pkgdown` site materials
    - Deployed to https://infer.tidymodels.org/


# infer 0.0.1
- Implemented the "intro stats" examples for randomization methods
