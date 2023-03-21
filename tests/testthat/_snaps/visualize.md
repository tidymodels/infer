# visualize basic tests

    Code
      hours_resamp %>% visualize(bins = "yep")
    Condition
      Error:
      ! `bins` must be 'numeric', not 'character'.

---

    Code
      gss_tbl %>% specify(sex ~ college, success = "female") %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "diff in props",
        order = c("no degree", "degree")) %>% visualize() + shade_p_value(direction = "both")
    Condition
      Error in `vapply()`:
      ! argument "obs_stat" is missing, with no default

---

    Code
      expect_warning(gss_tbl %>% specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>% generate(reps = 100, type = "permute") %>%
        calculate(stat = "diff in props", order = c("no degree", "degree")) %>%
        visualize(method = "both") + shade_p_value(direction = "both", obs_stat = obs_diff))
    Condition
      Error:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

---

    Code
      gss_tbl %>% specify(partyid ~ NULL) %>% hypothesize(null = "point", p = c(dem = 0.4,
        rep = 0.4, ind = 0.2)) %>% visualize(method = "traditional")
    Condition
      Error:
      ! Provide `method` with one of three options: `"theoretical"`, `"both"`, or `"simulation"`. `"simulation"` is the default for simulation-based null distributions, while `"theoretical"` is the only option for null distributions outputted by `assume()`.

---

    Code
      expect_warning(gss_tbl %>% specify(hours ~ sex) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "diff in means",
        order = c("female", "male")) %>% visualize(method = "both") + shade_p_value(
        direction = "both", obs_stat = obs_diff_mean))
    Condition
      Error:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

# method = "both" behaves nicely

    Code
      gss_tbl %>% specify(hours ~ NULL) %>% hypothesize(null = "point", mu = 4) %>%
        generate(reps = 100, type = "bootstrap") %>% visualize(method = "both")
    Condition
      Error:
      ! `generate()` and `calculate()` are both required to be done prior to `visualize(method = "both")`

# confidence interval plots are working

    Code
      gss_tbl_boot %>% visualize() + shade_confidence_interval(endpoints = df_error)
    Condition
      Error:
      ! Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector.

# visualize warns about removing `NaN`

    Code
      visualize(dist)
    Condition
      Error:
      ! All calculated statistics were `NaN`. See ?calculate for more details.

