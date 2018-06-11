# What we'd like to implement

## For future CRAN releases

- Check that corresponding z for one prop depends on `params` being set in `hypothesize` with `specify() %>% calculate()` shortcut
- Check that assumptions have been met for the theoretical distribution and `"both"`
- Fix double printing of `Response:` and `Explanatory:`
  - Might have something to do with `infer` class being set in multiple spots?
- Check that `specify() %>% calculate()` works for `stat = "z"` and `stat = "t"`
  - Determine if other wrapper functions should be created 
    - `z_test()`, `mean_stat()`, `diff_in_mean_stat()`, etc.? 
- Consider re-working how p-values can be calculated (both for computational and theoretical)
  - Create a `determine()` function with first argument being either `"p value"` or `"confidence interval"`? Maybe check to make sure that p-values don't exceed 1 too
  - Could have wrapper functions `p_value()` and `conf_inf()` as well
  - Should the wrapper functions like `t_test()` also include a logical `conf_int` argument?
- Check that `stat` is calculated appropriately if `generate()` is not called
  - `order` of the arguments into the standardized statistics should be checked again
  - May require moving of `set_params()` to be after `specify()`
  - Update `visualize()` tests to include `calculate()` step for `method = "theoretical"`
- Add `add_obs_stat` toggle into `visualize()` and p-value calculation?
- Shift to list-columns in `generate()`
- Implement check of `stat` in theoretical `visualize()`
- Write test to check that bootstrapped values are centered near the 
  hypothesized value in `specify()`
- Write vignettes on how NOT to use infer (strange errors, funky results, etc.)
- Implement theoretical distributions for bootstrap distributions
- Create `resources.md` with links to slides/talks/workshops given about `infer`
- Add Lionel's vis checking package ([`vdiffr`](https://github.com/lionel-/vdiffr)) to `visualize()` tests
- Shading for Confidence Intervals in `visualize()` (Mine would prefer green
for CI and red for p-values)
  - `direction = "between"` to get the green shading?
- Determine whether `calculate()` should be where the `set_params()` function is called
instead of in `specify()`
- Need to also add parameters to wrapper functions so that randomization
methods can be implemented by practitioners looking to skip the longer pipe syntax
- Find a better word than `"simulate"` for `type` in `generate()`
