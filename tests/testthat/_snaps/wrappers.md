# t_test works

    Code
      gss_tbl %>% t_test(response = "hours", explanatory = "sex")
    Condition
      Error:
      ! The response should be a bare variable name (not a string in quotation marks).

# chisq_test works

    Code
      chisq_test(x = gss_tbl, response = age, explanatory = partyid)
    Condition
      Error:
      ! The response variable of `age` is not appropriate since the response variable is expected to be categorical.

---

    Code
      chisq_test(x = gss_tbl, response = partyid, explanatory = age)
    Condition
      Error:
      ! The explanatory variable of `age` is not appropriate since the explanatory variable is expected to be categorical.

# _stat functions work

    Code
      expect_warning(chisq_stat(x = gss_tbl, response = age, explanatory = sex))
    Condition
      Error:
      ! The response variable of `age` is not appropriate since the response variable is expected to be categorical.

---

    Code
      expect_warning(chisq_stat(x = gss_tbl, response = sex, explanatory = age))
    Condition
      Error:
      ! The explanatory variable of `age` is not appropriate since the response variable is expected to be categorical.

# conf_int argument works

    Code
      gss_tbl %>% t_test(hours ~ sex, order = c("female", "male"), conf_int = TRUE,
      conf_level = 1.1)
    Condition
      Error:
      ! The `conf_level` argument must be a number between 0 and 1.

# two sample prop_test works

    Code
      prop_test(bad_df, resp ~ exp)
    Condition
      Warning in `anova.lm()`:
      ANOVA F-tests on an essentially perfect fit are unreliable
      Error:
      ! The response variable of `resp` is not appropriate
      since the response variable is expected to be categorical.

---

    Code
      prop_test(bad_df2, resp ~ exp)
    Condition
      Error:
      ! The explanatory variable of `exp` is not appropriate since the explanatory variable is expected to be categorical.

# one sample prop_test works

    Code
      prop_test(df_1, resp ~ NULL, p = 0.2, success = "b")
    Condition
      Error:
      ! b is not a valid level of resp.

