# direction is appropriate

    Code
      test_df %>% get_p_value(obs_stat = 0.5, direction = "righ")
    Condition
      Error:
      ! The provided value for `direction` is not appropriate. Possible values are "less", "greater", "two-sided", "left", "right", "both", "two_sided", "two sided", or "two.sided".

# theoretical p-value not supported error

    Code
      gss_tbl %>% specify(hours ~ partyid) %>% hypothesize(null = "independence") %>%
        calculate(stat = "F") %>% get_p_value(obs_stat = obs_F, direction = "right")
    Condition
      Error:
      ! Theoretical p-values are not yet supported. `x` should be the result of calling `generate()`.

# get_p_value throws error in case of `NaN` stat

    Code
      get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! 1 calculated statistic was `NaN`. Simulation-based p-values are not well-defined for null distributions with non-finite values. See ?calculate for more details.

---

    Code
      get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! 2 calculated statistics were `NaN`. Simulation-based p-values are not well-defined for null distributions with non-finite values. See ?calculate for more details.

---

    Code
      get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! All calculated statistics were `NaN`. See ?calculate for more details.

# get_p_value can handle fitted objects

    Code
      get_p_value(null_fits, obs_fit_2, "both")
    Condition
      Error:
      ! The explanatory variables used to generate the distribution of null fits are not the same used to fit the observed data.

---

    Code
      get_p_value(null_fits, obs_fit_3, "both")
    Condition
      Error:
      ! The response variable of the null fits (hours) is not the same as that of the observed fit (year).

# get_p_value can handle bad args with fitted objects

    Code
      get_p_value(null_fits, "boop", "both")
    Condition
      Error:
      ! The `obs_stat` argument should be the output of `fit()`. See the documentation with `?get_p_value`.

---

    Code
      get_p_value(null_fits, obs_fit$estimate, "both")
    Condition
      Error:
      ! The `obs_stat` argument should be the output of `fit()`. See the documentation with `?get_p_value`.

---

    Code
      get_p_value(obs_fit, null_fits, "both")
    Condition
      Error:
      ! The `x` argument needs to be passed to `generate()` before `fit()`.

# get_p_value errors informatively when args are switched

    Code
      get_p_value(obs_stat, null_dist, "both")
    Condition
      Error:
      ! It seems like the `obs_stat` argument has been passed to `get_p_value()` as the first argument when `get_p_value()` expects `x`, a distribution of statistics or coefficient estimates, as the first argument. Have you mistakenly switched the order of `obs_stat` and `x`?

