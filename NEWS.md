# infer 0.1.1.999
- Added `obs_stat` and `shade_direction` arguments to `visualize()`
  - Used internal `ggplot2` package functions suggested by @jimhester to shade
  density histogram appropriately
  - `bin.R` copied from `ggplot2` package for use here
  - Shading not currently available for `method = "theoretical"`
- Adding theoretical distributions to `visualize()` alone and as overlay with current implementations being
    - Two sample t
    - ANOVA F
    - One proportion z
    - Two proportion z
    
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
