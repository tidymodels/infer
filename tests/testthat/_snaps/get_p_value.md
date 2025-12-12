# direction is appropriate

    Code
      get_p_value(test_df, obs_stat = 0.5, direction = "righ")
    Condition
      Error in `get_p_value()`:
      ! The provided value for `direction` is not appropriate. Possible values are "less", "greater", "two-sided", "left", "right", "both", "two_sided", "two sided", or "two.sided".

# theoretical p-value not supported error

    Code
      get_p_value(calculate(hypothesize(specify(gss_tbl, hours ~ partyid), null = "independence"),
      stat = "F"), obs_stat = obs_F, direction = "right")
    Condition
      Error in `get_p_value()`:
      ! Theoretical p-values are not yet supported.
      i `x` should be the result of calling `generate()`.

# get_p_value warns in case of zero p-value

    Code
      res_ <- get_p_value(gss_calc, obs_stat = -10, direction = "left")
    Condition
      Warning:
      Please be cautious in reporting a p-value of 0. This result is an approximation based on the number of `reps` chosen in the `generate()` step.
      i See `get_p_value()` (`?infer::get_p_value()`) for more information.

# get_p_value throws error in case of `NaN` stat

    Code
      res_ <- get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! 1 calculated statistic was `NaN`. Simulation-based p-values are not well-defined for null distributions with non-finite values.
      i See `calculate()` (`?infer::calculate()`) for more details.

---

    Code
      res_ <- get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! 2 calculated statistics were `NaN`. Simulation-based p-values are not well-defined for null distributions with non-finite values.
      i See `calculate()` (`?infer::calculate()`) for more details.

---

    Code
      res_ <- get_p_value(gss_calc, 0, "both")
    Condition
      Error:
      ! All calculated statistics were `NaN`.
      i See `calculate()` (`?infer::calculate()`) for more details.

# get_p_value can handle fitted objects

    Code
      get_p_value(null_fits, obs_fit_2, "both")
    Condition
      Error in `get_p_value()`:
      ! The explanatory variables used to generate the distribution of null fits are not the same used to fit the observed data.

---

    Code
      get_p_value(null_fits, obs_fit_3, "both")
    Condition
      Error in `get_p_value()`:
      ! The response variable of the null fits (hours) is not the same as that of the observed fit (year).

# get_p_value can handle bad args with fitted objects

    Code
      get_p_value(null_fits, "boop", "both")
    Condition
      Error in `get_p_value()`:
      ! The `obs_stat` argument should be the output of `fit()`.
      i See the documentation with `?get_p_value`.

---

    Code
      get_p_value(null_fits, obs_fit$estimate, "both")
    Condition
      Error in `get_p_value()`:
      ! The `obs_stat` argument should be the output of `fit()`.
      i See the documentation with `?get_p_value`.

---

    Code
      get_p_value(obs_fit, null_fits, "both")
    Condition
      Error in `get_p_value()`:
      ! The `x` argument needs to be passed to `generate()` before `fit()`.

# get_p_value errors informatively when args are switched

    Code
      get_p_value(obs_stat, null_dist, "both")
    Condition
      Error in `get_p_value()`:
      ! It seems like the `obs_stat` argument has been passed to `get_p_value()` as the first argument when `get_p_value()` expects `x`, a distribution of statistics or coefficient estimates, as the first argument.
      i Have you mistakenly switched the order of `obs_stat` and `x`?

# get_p_value can handle theoretical distributions

    Code
      old_way <- chisq_test(gss, college ~ finrela)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

# get_p_value warns with bad theoretical distributions

    Code
      res_ <- get_p_value(t_dist_30, t_obs, direction = "both")
    Condition
      Warning:
      `x` and `obs_stat` were generated using different null hypotheses. This workflow is untested and results may not mean what you think they mean.

