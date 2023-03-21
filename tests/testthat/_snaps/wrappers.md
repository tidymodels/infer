# t_test works

    Code
      res_ <- gss_tbl %>% t_test(hours ~ sex)
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "male" - "female", or divided in the order "male" / "female" for ratio-based statistics. To specify this order yourself, supply `order = c("male", "female")`.

---

    Code
      gss_tbl %>% t_test(response = "hours", explanatory = "sex")
    Condition
      Error in `parse_variables()`:
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
      res_ <- gss_tbl %>% chisq_stat(college ~ partyid)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way <- gss_tbl %>% chisq_stat(college ~ partyid)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way <- gss_tbl %>% chisq_stat(partyid ~ NULL)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way_alt <- gss_tbl %>% chisq_stat(response = partyid)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      res_ <- gss_tbl %>% t_stat(hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way <- gss_tbl %>% t_stat(hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way_alt <- gss_tbl %>% t_stat(response = hours, explanatory = sex,
        order = c("male", "female"))
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      res_ <- gss_tbl %>% t_stat(hours ~ NULL)
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way <- gss_tbl %>% t_stat(hours ~ NULL)
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      obs_stat_way_alt <- gss_tbl %>% t_stat(response = hours)
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      res_ <- chisq_stat(x = gss_tbl, response = age, explanatory = sex)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.
      Error in `chisq_stat()`:
      ! The response variable of `age` is not appropriate since the response variable is expected to be categorical.

---

    Code
      res_ <- chisq_stat(x = gss_tbl, response = sex, explanatory = age)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.
      Error in `chisq_stat()`:
      ! The explanatory variable of `age` is not appropriate since the response variable is expected to be categorical.

# conf_int argument works

    Code
      res_ <- gss_tbl %>% t_test(hours ~ sex, order = c("female", "male"), conf_int = TRUE,
      conf_level = 1.1)
    Condition
      Error in `t_test()`:
      ! The `conf_level` argument must be a number between 0 and 1.

---

    Code
      no_var_equal <- gss_tbl_small %>% t_stat(hours ~ sex, order = c("female",
        "male"))
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      var_equal <- gss_tbl_small %>% t_stat(hours ~ sex, order = c("female", "male"),
      var.equal = TRUE)
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

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

# wrappers can handled ordered factors

    Code
      ordered_t_1 <- gss_tbl %>% dplyr::mutate(income = factor(income, ordered = TRUE)) %>%
        chisq_test(income ~ partyid)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_f_1 <- gss_tbl %>% dplyr::mutate(income = factor(income, ordered = FALSE)) %>%
        chisq_test(income ~ partyid)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_t_2 <- gss_tbl %>% dplyr::mutate(income = factor(income, ordered = TRUE)) %>%
        chisq_test(partyid ~ income)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

---

    Code
      ordered_f_2 <- gss_tbl %>% dplyr::mutate(income = factor(income, ordered = FALSE)) %>%
        chisq_test(partyid ~ income)
    Condition
      Warning in `stats::chisq.test()`:
      Chi-squared approximation may be incorrect

