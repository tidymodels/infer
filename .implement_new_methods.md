1. Check that `data %>% specify()` gives appropriate variable types
1. Check that `attr(data, "theory_type")` is set appropriately
  - If not, write code to add it in `set_params.R` based on variable types also 
  set as `attr`ibutes
1. Create `# Testing` as last section of `# Implemented` and move appropriate
chunks of `*both_methods.Rmd` file there up to "Implemented"  
1. Write code to calculate theory-based statistic (t, z, chi-square, etc.) in `calculate.R`
1. Write code to add theoretical distribution in `visualize.R`
1. Delete `# Testing` above new implementation
1. Run `devtools::check()`
  - Add undefined global functions or variables to `infer.R` `globalVariables`
  - Run `devtools::check()` again until 0 errors | 0 warnings | 0 notes
1. Run `pkgdown::build_site()`
1. Commit files to GitHub and continue

