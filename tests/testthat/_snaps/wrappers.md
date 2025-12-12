# t_test works

    Code
      res_ <- t_test(gss_tbl, hours ~ sex)
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "male" - "female", or divided in the order "male" / "female" for ratio-based statistics. To specify this order yourself, supply `order = c("male", "female")`.

---

    Code
      t_test(gss_tbl, response = "hours", explanatory = "sex")
    Condition
      Error in `t_test()`:
      ! The response should be a bare variable name (not a string in quotation marks).

# chisq_test works

    Code
      chisq_test(x = gss_tbl, response = age, explanatory = partyid)
    Condition
      Error in `chisq_test()`:
      ! The response variable of `age` is not appropriate since the response variable is expected to be categorical.

---

    Code
      chisq_test(x = gss_tbl, response = partyid, explanatory = age)
    Condition
      Error in `chisq_test()`:
      ! The explanatory variable of `age` is not appropriate since the explanatory variable is expected to be categorical.

# _stat functions work

    Code
      res_ <- chisq_stat(gss_tbl, college ~ partyid)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way <- chisq_stat(gss_tbl, college ~ partyid)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way <- chisq_stat(gss_tbl, partyid ~ NULL)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way_alt <- chisq_stat(gss_tbl, response = partyid)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      res_ <- t_stat(gss_tbl, hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way <- t_stat(gss_tbl, hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way_alt <- t_stat(gss_tbl, response = hours, explanatory = sex, order = c(
        "male", "female"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      res_ <- t_stat(gss_tbl, hours ~ NULL)
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way <- t_stat(gss_tbl, hours ~ NULL)
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      obs_stat_way_alt <- t_stat(gss_tbl, response = hours)
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      res_ <- chisq_stat(x = gss_tbl, response = age, explanatory = sex)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.
      Error in `chisq_stat()`:
      ! The response variable of `age` is not appropriate since the response variable is expected to be categorical.

---

    Code
      res_ <- chisq_stat(x = gss_tbl, response = sex, explanatory = age)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.
      Error in `chisq_stat()`:
      ! The explanatory variable of `age` is not appropriate since the response variable is expected to be categorical.

# conf_int argument works

    Code
      res_ <- t_test(gss_tbl, hours ~ sex, order = c("female", "male"), conf_int = TRUE,
      conf_level = 1.1)
    Condition
      Error in `t_test()`:
      ! The `conf_level` argument must be a number between 0 and 1.

---

    Code
      no_var_equal <- t_stat(gss_tbl_small, hours ~ sex, order = c("female", "male"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      var_equal <- t_stat(gss_tbl_small, hours ~ sex, order = c("female", "male"),
      var.equal = TRUE)
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

# two sample prop_test works

    Code
      res_ <- prop_test(df, resp ~ exp)
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "a" - "b", or divided in the order "a" / "b" for ratio-based statistics. To specify this order yourself, supply `order = c("a", "b")`.

---

    Code
      res_ <- prop_test(bad_df, resp ~ exp)
    Condition
      Warning in `anova.lm()`:
      ANOVA F-tests on an essentially perfect fit are unreliable
      Error in `prop_test()`:
      ! The response variable of `resp` is not appropriate since the response variable is expected to be categorical.

---

    Code
      res_ <- prop_test(bad_df2, resp ~ exp)
    Condition
      Error in `prop_test()`:
      ! The explanatory variable of `exp` is not appropriate since the explanatory variable is expected to be categorical.

# one sample prop_test works

    Code
      res_ <- prop_test(df_1, resp ~ NULL)
    Message
      No `p` argument was hypothesized, so the test will assume a null hypothesis `p = .5`.

---

    Code
      res_ <- prop_test(df_1, resp ~ NULL, p = 0.2, success = "b")
    Condition
      Error in `prop_test()`:
      ! b is not a valid level of resp.

# prop_test handles >2 explanatory levels gracefully

    Code
      res_2 <- prop_test(dfr, resp ~ exp, order = c("a", "b"))
    Condition
      Warning:
      The `order` argument will be ignored as it is not well-defined for explanatory variables with more than 2 levels.
      i To silence this message, avoid passing the `order` argument.

---

    Code
      res_3 <- prop_test(dfr, resp ~ exp, order = c("a", "b", "c"))
    Condition
      Warning:
      The `order` argument will be ignored as it is not well-defined for explanatory variables with more than 2 levels.
      i To silence this message, avoid passing the `order` argument.

# prop_test errors with >2 response levels

    Code
      res_1 <- prop_test(dfr, resp ~ exp)
    Condition
      Error in `prop_test()`:
      ! This test is not defined for response variables with more than 2 levels.

# wrappers can handled ordered factors

    Code
      ordered_t_1 <- chisq_test(dplyr::mutate(gss_tbl, income = factor(income,
        ordered = TRUE)), income ~ partyid)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_f_1 <- chisq_test(dplyr::mutate(gss_tbl, income = factor(income,
        ordered = FALSE)), income ~ partyid)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_t_2 <- chisq_test(dplyr::mutate(gss_tbl, income = factor(income,
        ordered = TRUE)), partyid ~ income)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_f_2 <- chisq_test(dplyr::mutate(gss_tbl, income = factor(income,
        ordered = FALSE)), partyid ~ income)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

