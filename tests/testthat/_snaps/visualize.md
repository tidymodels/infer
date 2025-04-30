# visualize warns with bad arguments

    Code
      res_ <- visualize(calculate(generate(hypothesize(specify(gss_tbl, age ~ hours),
      null = "independence"), reps = 100, type = "permute"), stat = "slope"),
      obs_stat = obs_slope, direction = "right")
    Condition
      Warning:
      The arguments `c("obs_stat", "direction")` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res_ <- visualize(calculate(generate(hypothesize(specify(gss_tbl, age ~ hours),
      null = "independence"), reps = 100, type = "permute"), stat = "slope"),
      obs_stat = obs_slope)
    Condition
      Warning:
      The arguments `obs_stat` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res_ <- visualize(calculate(generate(hypothesize(specify(gss_tbl, age ~ hours),
      null = "independence"), reps = 100, type = "permute"), stat = "slope"),
      endpoints = c(0.01, 0.02))
    Condition
      Warning:
      The arguments `endpoints` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

---

    Code
      res <- visualize(age_hours_df, endpoints = c(0.01, 0.02))
    Condition
      Warning:
      The arguments `endpoints` are deprecated in `visualize()` and will be ignored. They should now be passed to one of `shade_p_value()` or `shade_confidence_interval()`.

# visualize basic tests

    Code
      visualize(hours_resamp, bins = "yep")
    Condition
      Error in `visualize()`:
      ! `bins` must be 'numeric', not 'character'.

---

    argument "obs_stat" is missing, with no default

---

    Code
      res_vis_theor_none_1 <- visualize(calculate(hypothesize(specify(gss_tbl, sex ~
        college, success = "female"), null = "independence"), stat = "z", order = c(
        "no degree", "degree")), method = "theoretical")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

---

    Code
      visualize(calculate(generate(hypothesize(specify(gss_tbl, sex ~ college,
      success = "female"), null = "independence"), reps = 100, type = "permute"),
      stat = "diff in props", order = c("no degree", "degree")), method = "both") +
        shade_p_value(direction = "both", obs_stat = obs_diff)
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.
      Error in `theoretical_layer()`:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

---

    Code
      visualize(hypothesize(specify(gss_tbl, partyid ~ NULL), null = "point", p = c(
        dem = 0.4, rep = 0.4, ind = 0.2)), method = "traditional")
    Condition
      Error in `visualize()`:
      ! Provide `method` with one of three options: `"theoretical"`, `"both"`, or `"simulation"`. `"simulation"` is the default for simulation-based null distributions, while `"theoretical"` is the only option for null distributions outputted by `assume()`.

---

    Code
      visualize(calculate(generate(hypothesize(specify(gss_tbl, hours ~ sex), null = "independence"),
      reps = 100, type = "permute"), stat = "diff in means", order = c("female",
        "male")), method = "both") + shade_p_value(direction = "both", obs_stat = obs_diff_mean)
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.
      Error in `theoretical_layer()`:
      ! Your `calculate`d statistic and the theoretical distribution are on different scales. Use a standardized `stat` instead.

---

    Code
      res_vis_theor_both_1 <- visualize(calculate(generate(hypothesize(specify(
        gss_tbl, hours ~ sex), null = "independence"), reps = 100, type = "permute"),
      stat = "diff in means", order = c("female", "male")), method = "theoretical") +
        shade_p_value(direction = "both", obs_stat = obs_diff_mean)
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.
      Warning:
      Your `calculate`d statistic and the theoretical distribution are on different scales. Displaying only the theoretical distribution.

# method = "both" behaves nicely

    Code
      visualize(generate(hypothesize(specify(gss_tbl, hours ~ NULL), null = "point",
      mu = 4), reps = 100, type = "bootstrap"), method = "both")
    Condition
      Error in `visualize()`:
      ! `generate()` and `calculate()` are both required to be done prior to `visualize(method = "both")`

---

    Code
      res_method_both <- visualize(calculate(generate(hypothesize(specify(gss_tbl,
        hours ~ college), null = "point", mu = 4), reps = 10, type = "bootstrap"),
      stat = "t", order = c("no degree", "degree")), method = "both")
    Condition
      Warning:
      With only 10 replicates, it may be difficult to see the relationship between simulation and theory.
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

# Traditional right-tailed tests have warning if not right-tailed

    Code
      res_ <- visualize(calculate(generate(hypothesize(specify(gss_tbl, sex ~ partyid,
      success = "female"), null = "independence"), reps = 100, type = "permute"),
      stat = "Chisq"), method = "both") + shade_p_value(obs_stat = 2, direction = "left")
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

---

    Code
      res_ <- visualize(calculate(generate(hypothesize(specify(gss_tbl, age ~ partyid),
      null = "independence"), reps = 100, type = "permute"), stat = "F"), method = "both") +
        shade_p_value(obs_stat = 2, direction = "two_sided")
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

---

    Code
      res_ <- visualize(calculate(hypothesize(specify(gss_tbl, sex ~ partyid,
      success = "female"), null = "independence"), stat = "Chisq"), method = "theoretical") +
        shade_p_value(obs_stat = 2, direction = "left")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

---

    Code
      res_ <- visualize(calculate(hypothesize(specify(gss_tbl, age ~ partyid), null = "independence"),
      stat = "F"), method = "theoretical") + shade_p_value(obs_stat = 2, direction = "two_sided")
    Message
      Rather than setting `method = "theoretical"` with a simulation-based null distribution, the preferred method for visualizing theory-based distributions with infer is now to pass the output of `assume()` as the first argument to `visualize()`.
    Condition
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

# confidence interval plots are working

    Code
      res_ <- visualize(gss_tbl_boot) + shade_confidence_interval(endpoints = df_error)
    Condition
      Error in `shade_confidence_interval()`:
      ! Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector.

---

    Code
      res_ <- visualize(gss_tbl_boot) + shade_confidence_interval(endpoints = vec_error)
    Condition
      Warning:
      Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. Using the first two entries as the `endpoints`.

---

    Code
      res_ci_vis <- visualize(gss_tbl_boot) + shade_confidence_interval(endpoints = perc_ci,
        direction = "between")
    Condition
      Warning:
      Ignoring unknown parameters: `direction`
      Warning:
      Ignoring unknown parameters: `direction`

# title adapts to not hypothesis testing workflow

    Code
      res_vis_no_hypothesize_both <- visualize(calculate(gss_tbl_boot_tbl, stat = "t"),
      method = "both")
    Condition
      Warning:
      A t statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null value: `mu = 0`.
      Warning:
      Check to make sure the conditions have been met for the theoretical method. infer currently does not check these for you.

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
      1 calculated statistic was `NaN`. `NaN`s have been omitted from visualization.
      i See `calculate()` (`?infer::calculate()`) for more details.

---

    Code
      res_ <- visualize(dist)
    Condition
      Warning:
      2 calculated statistics were `NaN`. `NaN`s have been omitted from visualization.
      i See `calculate()` (`?infer::calculate()`) for more details.

---

    Code
      res_ <- visualize(dist)
    Condition
      Error:
      ! All calculated statistics were `NaN`.
      i See `calculate()` (`?infer::calculate()`) for more details.

# visualize can handle multiple explanatory variables

    Code
      res_viz_fit_p_val_right <- visualize(null_fits) + shade_p_value(obs_stat = obs_fit,
        direction = "right")

# visualize can handle `assume()` output

    Code
      res_viz_assume_t_sim <- visualize(null_dist, method = "simulation")
    Condition
      Warning:
      Simulation-based visualization methods are not well-defined for `assume()` output; the `method` argument will be ignored.
      i Set `method = "theoretical"` to silence this message.

---

    Code
      res_viz_assume_t_both <- visualize(null_dist, method = "both")
    Condition
      Warning:
      Simulation-based visualization methods are not well-defined for `assume()` output; the `method` argument will be ignored.
      i Set `method = "theoretical"` to silence this message.

