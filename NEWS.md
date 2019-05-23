# Development version

## Breaking changes

- `shade_confidence_interval()` now plots vertical lines starting from zero (previously - from the bottom of a plot).
- `shade_p_value()` now uses "area under the curve" approach to shading.

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
- Make transparancy in `visualize()` to not depend on method and data volume.
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
    - Deployed to https://infer.netlify.com


# infer 0.0.1
- Implemented the "intro stats" examples for randomization methods
