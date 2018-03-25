# infer 0.1.1.9000

- Switched to `!!` instead of `UQ()` since `UQ()` is deprecated in {rlang} 0.2.0
- Added wrapper functions `t_test()` and `chisq_test()` that use a formula interface
- Added `obs_stat` and `shade_direction` arguments to `visualize()`
- Added check so that bar graph created with `visualize()` if number of unique
values for generated statistics is small
<!--
  - Used internal `ggplot2` package functions suggested by @jimhester to shade
  density histogram appropriately
  - `bin.R` copied from `ggplot2` package for use here
-->
- Adding theoretical distributions to `visualize()` alone and as overlay with current implementations being
    - Two sample t
    - ANOVA F
    - One proportion z
    - Two proportion z
    - Chi-square test of independence
    - Chi-square Goodness of Fit test
    - Standardized slope (t)

## To Dos

- Need to also add parameters to wrapper functions so that randomization methods can be implemented
by practictioners looking to skip the longer pipe syntax
- Determine if other wrapper functions should be created 
    - `z_test()`? 
    - Add implementation of one sample `t_test()`?
- Shading not currently available for `method = "theoretical"` 
- Need to convert the `visualize()` functions into methods similar to what was done in `calculate()`
    
# infer 0.1.1
- Added additional tests
- Fixed bugs post-CRAN release
- Automated travis build of pkgdown to gh-pages branch

# infer 0.1.0
- Altered the way that successes are indicated in an infer pipeline. They now live in `specify()`.
- Updated documentation with examples
- Created `pkgdown` site materials
  - Deployed to https://infer.netlify.com


# infer 0.0.1
- Implemented the "intro stats" examples for randomization methods as shown on the README
