# visualize warns with bad arguments

    Code
      res_ <- gss_tbl %>% specify(age ~ hours) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "slope") %>%
        visualize(obs_stat = obs_slope, direction = "right")
    Condition
      Warning:
      The arguments `c("obs_stat", "direction")` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res_ <- gss_tbl %>% specify(age ~ hours) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "slope") %>%
        visualize(obs_stat = obs_slope)
    Condition
      Warning:
      The arguments `obs_stat` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res_ <- gss_tbl %>% specify(age ~ hours) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "slope") %>%
        visualize(endpoints = c(0.01, 0.02))
    Condition
      Warning:
      The arguments `endpoints` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res <- age_hours_df %>% visualize(endpoints = c(0.01, 0.02))
    Condition
      Warning:
      The arguments `endpoints` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

# visualize basic tests

    Code
      hours_resamp %>% visualize(bins = "yep")
    Condition
      Error in `visualize()`:
      ! `bins` must be 'numeric', not 'character'.

---

    argument "obs_stat" is missing, with no default

---

    Code
      res_vis_theor_none_1 <- gss_tbl %>% specify(sex ~ college, success = "female") %>%
        hypothesize(null = "independence") %>% calculate(stat = "z", order = c(
        "no degree", "degree")) %>% visualize(method = "theoretical")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

---

    Code
      gss_tbl %>% specify(sex ~ college, success = "female") %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "diff in props",
        order = c("no degree", "degree")) %>% visualize(method = "both") +
        shade_p_value(direction = "both", obs_stat = obs_diff)
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.
      Error in `theoretical_layer()`:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

---

    Code
      gss_tbl %>% specify(partyid ~ NULL) %>% hypothesize(null = "point", p = c(dem = 0.4,
        rep = 0.4, ind = 0.2)) %>% visualize(method = "traditional")
    Condition
      Error in `visualize()`:
      ! Provide `method` with one of three options: `"theoretical"`, `"both"`, or `"simulation"`. `"simulation"` is the default for simulation-based null distributions, while `"theoretical"` is the only option for null distributions outputted by `assume()`.

---

    Code
      gss_tbl %>% specify(hours ~ sex) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "diff in means",
        order = c("female", "male")) %>% visualize(method = "both") + shade_p_value(
        direction = "both", obs_stat = obs_diff_mean)
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.
      Error in `theoretical_layer()`:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

---

    Code
      res_vis_theor_both_1 <- gss_tbl %>% specify(hours ~ sex) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "diff in means",
        order = c("female", "male")) %>% visualize(method = "theoretical") +
        shade_p_value(direction = "both", obs_stat = obs_diff_mean)
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.
      Warning:
      Your `calculate`d statistic and the theoretical distribution are on different scales. Displaying only the theoretical distribution.

# method = "both" behaves nicely

    Code
      gss_tbl %>% specify(hours ~ NULL) %>% hypothesize(null = "point", mu = 4) %>%
        generate(reps = 100, type = "bootstrap") %>% visualize(method = "both")
    Condition
      Error in `visualize()`:
      ! `generate()` and `calculate()` are both required to be done prior to `visualize(method = "both")`

---

    Code
      res_method_both <- gss_tbl %>% specify(hours ~ college) %>% hypothesize(null = "point",
        mu = 4) %>% generate(reps = 10, type = "bootstrap") %>% calculate(stat = "t",
        order = c("no degree", "degree")) %>% visualize(method = "both")
    Condition
      Warning:
      With only 10 replicates, it may be difficult to see the relationship between simulation and theory.
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

# Traditional right-tailed tests have warning if not right-tailed

    Code
      res_ <- gss_tbl %>% specify(sex ~ partyid, success = "female") %>% hypothesize(
        null = "independence") %>% generate(reps = 100, type = "permute") %>%
        calculate(stat = "Chisq") %>% visualize(method = "both") + shade_p_value(
        obs_stat = 2, direction = "left")
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

---

    Code
      res_ <- gss_tbl %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        generate(reps = 100, type = "permute") %>% calculate(stat = "F") %>%
        visualize(method = "both") + shade_p_value(obs_stat = 2, direction = "two_sided")
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

---

    Code
      res_ <- gss_tbl %>% specify(sex ~ partyid, success = "female") %>% hypothesize(
        null = "independence") %>% calculate(stat = "Chisq") %>% visualize(method = "theoretical") +
        shade_p_value(obs_stat = 2, direction = "left")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

---

    Code
      res_ <- gss_tbl %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        calculate(stat = "F") %>% visualize(method = "theoretical") + shade_p_value(
        obs_stat = 2, direction = "two_sided")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

# confidence interval plots are working

    Code
      res_ <- gss_tbl_boot %>% visualize() + shade_confidence_interval(endpoints = df_error)
    Condition
      Error in `shade_confidence_interval()`:
      ! Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector.

---

    Code
      res_ <- gss_tbl_boot %>% visualize() + shade_confidence_interval(endpoints = vec_error)
    Condition
      Warning:
      Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. Using the first two entries as the `endpoints`.

---

    Code
      res_ci_vis <- gss_tbl_boot %>% visualize() + shade_confidence_interval(
        endpoints = perc_ci, direction = "between")
    Condition
      Warning:
      Ignoring unknown parameters: `direction`
      Warning:
      Ignoring unknown parameters: `direction`

# title adapts to not hypothesis testing workflow

    Code
      res_vis_no_hypothesize_both <- gss_tbl_boot_tbl %>% calculate(stat = "t") %>%
        visualize(method = "both")
    Condition
      Warning:
      A t statistic requires a null hypothesis to calculate the observed statistic. 
      Output assumes the following null value: `mu = 0`.
      Warning:
      Check to make sure the conditions have been met for the theoretical method. {infer} currently does not check these for you.

# warn_right_tail_test works

    Code
      warn_right_tail_test("left", stat_name)
    Condition
      Warning:
      F usually corresponds to right-tailed tests. Proceed with caution.
    Output
      [1] TRUE

---

    Code
      warn_right_tail_test("two_sided", stat_name)
    Condition
      Warning:
      F usually corresponds to right-tailed tests. Proceed with caution.
    Output
      [1] TRUE

---

    Code
      warn_right_tail_test("left", stat_name)
    Condition
      Warning:
      Chi-Square usually corresponds to right-tailed tests. Proceed with caution.
    Output
      [1] TRUE

---

    Code
      warn_right_tail_test("two_sided", stat_name)
    Condition
      Warning:
      Chi-Square usually corresponds to right-tailed tests. Proceed with caution.
    Output
      [1] TRUE

# visualize warns about removing `NaN`

    Code
      res_ <- visualize(dist)
    Condition
      Warning:
      1 calculated statistic was `NaN`. `NaN`s have been omitted from visualization. See ?calculate for more details.

---

    Code
      res_ <- visualize(dist)
    Condition
      Warning:
      2 calculated statistics were `NaN`. `NaN`s have been omitted from visualization. See ?calculate for more details.

---

    Code
      res_ <- visualize(dist)
    Condition
      Error:
      ! All calculated statistics were `NaN`. See ?calculate for more details.

# visualize can handle multiple explanatory variables

    Code
      res_viz_fit_p_val_right <- null_fits %>% visualize() + shade_p_value(obs_stat = obs_fit,
        direction = "right")

# visualize can handle `assume()` output

    Code
      res_viz_assume_t_sim <- visualize(null_dist, method = "simulation")
    Condition
      Warning:
      Simulation-based visualization methods are not well-defined for `assume()` output; the `method` argument will be ignored. Set `method = "theoretical"` to silence this message.

---

    Code
      res_viz_assume_t_both <- visualize(null_dist, method = "both")
    Condition
      Warning:
      Simulation-based visualization methods are not well-defined for `assume()` output; the `method` argument will be ignored. Set `method = "theoretical"` to silence this message.

