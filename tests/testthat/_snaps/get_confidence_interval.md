# get_confidence_interval messages with no explicit `level`

    Code
      res_ <- get_confidence_interval(test_df)
    Message
      Using `level = 0.95` to compute confidence interval.

# get_confidence_interval checks input

    Code
      test_df %>% get_confidence_interval(type = "other")
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! The options for `type` are "percentile", "se", or "bias-corrected".

---

    Code
      test_df %>% get_confidence_interval(level = 1.2)
    Condition
      Error:
      ! The value of `level` must be between 0 and 1 non-inclusive.

---

    Code
      test_df %>% get_confidence_interval(point_estimate = "a")
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! `point_estimate` must be 'numeric', not 'character'.

---

    Code
      test_df %>% get_confidence_interval(type = "se", point_estimate = "a")
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! `point_estimate` must be 'numeric', not 'character'.

---

    Code
      test_df %>% get_confidence_interval(type = "se", point_estimate = data.frame(p = "a"))
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! `point_estimate[[1]][[1]]` must be 'numeric', not 'character'.

---

    Code
      test_df %>% get_confidence_interval(type = "se")
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! A numeric value needs to be given for `point_estimate` for `type` "se" or "bias-corrected".

---

    Code
      test_df %>% get_confidence_interval(type = "bias-corrected")
    Message
      Using `level = 0.95` to compute confidence interval.
    Condition
      Error:
      ! A numeric value needs to be given for `point_estimate` for `type` "se" or "bias-corrected".

# get_confidence_interval can handle fitted objects

    Code
      get_confidence_interval(null_fits, point_estimate = obs_fit_2, level = 0.95)
    Condition
      Error:
      ! The explanatory variables used to generate the distribution of null fits are not the same used to fit the observed data.

---

    Code
      get_confidence_interval(null_fits, point_estimate = obs_fit_3, level = 0.95)
    Condition
      Error:
      ! The response variable of the null fits (hours) is not the same as that of the observed fit (year).

# get_confidence_interval can handle bad args with fitted objects

    Code
      get_confidence_interval(null_fits, point_estimate = "boop", level = 0.95)
    Condition
      Error:
      ! The `point_estimate` argument should be the output of `fit()`. See the documentation with `?get_confidence_interval`.

---

    Code
      get_confidence_interval(null_fits, point_estimate = obs_fit$estimate, level = 0.95)
    Condition
      Error:
      ! The `point_estimate` argument should be the output of `fit()`. See the documentation with `?get_confidence_interval`.

---

    Code
      get_confidence_interval(obs_fit, point_estimate = null_fits, level = 0.95)
    Condition
      Error:
      ! The `x` argument needs to be passed to `generate()` before `fit()`.

# theoretical CIs check arguments properly

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, type = "percentile",
        point_estimate = x_bar)
    Condition
      Error:
      ! The only `type` option for theory-based confidence intervals is `type = "se"`.

---

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, type = "boop",
        point_estimate = x_bar)
    Condition
      Error:
      ! The only `type` option for theory-based confidence intervals is `type = "se"`.

---

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, point_estimate = dplyr::pull(
        x_bar))
    Condition
      Error:
      ! For theoretical confidence intervals, the `point_estimate` argument must be an `infer` object. Have you made sure to supply the output of `calculate()` as the `point_estimate` argument?

---

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, point_estimate = x_bar$
        stat)
    Condition
      Error:
      ! For theoretical confidence intervals, the `point_estimate` argument must be an `infer` object. Have you made sure to supply the output of `calculate()` as the `point_estimate` argument?

---

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, point_estimate = obs_t)
    Condition
      Error:
      ! The only allowable statistics for theoretical confidence intervals are "mean", "prop", "diff in means", and "diff in props". See the "Details" section of `?get_confidence_interval` for more details.

---

    Code
      get_confidence_interval(null_dist_theory, level = 0.95, point_estimate = p_hat)
    Condition
      Error:
      ! Confidence intervals using a `t` distribution for `stat = prop` are not implemented.

---

    Code
      get_confidence_interval(null_dist_z, level = 0.95, point_estimate = x_bar)
    Condition
      Error:
      ! Confidence intervals using a `z` distribution for `stat = mean` are not implemented.

