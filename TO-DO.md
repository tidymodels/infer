## For next CRAN version

- Write tests to get `covr::package_coverage()` back up over 90%
  - Check for `visualize()` producing appropriate plots if `generate()` is or is
  not done
  - Write test to check that bootstrapped values are centered near the 
  hypothesized value in `specify()`
- Decide on how to handle NA's

## For future versions

- Shading for Confidence Intervals in `visualize()` (Mine would prefer green
for CI and red for p-values)
- Need to also add parameters to wrapper functions so that randomization
methods can be implemented by practitioners looking to skip the longer pipe syntax
- Determine if other wrapper functions should be created 
    - `z_test()`? 
    - Add implementation of one sample `t_test()`?
- Create easy functions to determine observed statistics like the implemented
`t_stat`: e.g., `mean_stat`?
- Implement theoretical distributions for bootstrap distributions