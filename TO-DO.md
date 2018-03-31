## For upcoming CRAN version

- Write tests to get `covr::package_coverage()` back up over 90%
  - Check for `visualize()` producing appropriate plots if `generate()` is or is
  not done
- Write test to check that bootstrapped values are centered near the 
  hypothesized value in `specify()`
- Decide on how to handle NA's

## For future versions

- Implement theoretical distributions for bootstrap distributions
- Create `references.md` with links to slides/talks/workshops given about `infer`
- Produce error is `mu` is given in `specify()` but `stat = "median"` provided in `calculate()`
- Shading for Confidence Intervals in `visualize()` (Mine would prefer green
for CI and red for p-values)
- Determine whether `calculate()` should be where the `set_params()` function is called
instead of in `specify()`
- Need to also add parameters to wrapper functions so that randomization
methods can be implemented by practitioners looking to skip the longer pipe syntax
- Determine if other wrapper functions should be created 
    - `z_test()`? 
    - Add implementation of one sample `t_test()`?
- Create easy functions to determine observed statistics like the implemented
`t_stat`: e.g., `mean_stat`?
- Tweak `pkgdown` page to include ToDo's. [{dplyr}](https://github.com/tidyverse/dplyr/blob/master/_pkgdown.yml) example