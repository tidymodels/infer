# x is a tibble

    Code
      calculate(vec, stat = "mean")
    Condition
      Error:
      ! `x` must be 'tibble', not 'integer'.

# calculate checks `stat` argument

    Code
      calculate(gss_tbl, stat = 3)
    Condition
      Error:
      ! `stat` must be 'string', not 'double'.

---

    Code
      calculate(gen_gss_slope, stat = "slopee")
    Condition
      Error in `check_calculate_stat()`:
      ! `stat` must be one of "mean", "median", "sum", "sd", "prop", "count", "diff in means", "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z", "ratio of props", "ratio of means", or "odds ratio", not "slopee".
      i Did you mean "slope"?

---

    Code
      calculate(gen_gss_slope, stat = "stdev")
    Condition
      Error in `check_calculate_stat()`:
      ! `stat` must be one of "mean", "median", "sum", "sd", "prop", "count", "diff in means", "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z", "ratio of props", "ratio of means", or "odds ratio", not "stdev".

---

    Code
      calculate(gen_gss_slope, stat = "stat")
    Condition
      Error in `check_calculate_stat()`:
      ! `stat` must be one of "mean", "median", "sum", "sd", "prop", "count", "diff in means", "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z", "ratio of props", "ratio of means", or "odds ratio", not "stat".

---

    Code
      calculate(gen_gss_slope, stat = "chi sq")
    Condition
      Error in `check_calculate_stat()`:
      ! `stat` must be one of "mean", "median", "sum", "sd", "prop", "count", "diff in means", "diff in medians", "diff in props", "Chisq", "F", "slope", "correlation", "t", "z", "ratio of props", "ratio of means", or "odds ratio", not "chi sq".
      i Did you mean "Chisq"?

# errors informatively with incompatible stat vs hypothesis

    Code
      gss %>% specify(college ~ sex, success = "degree") %>% hypothesise(null = "point",
        p = 0.4) %>% calculate(stat = "diff in props", order = c("female", "male"))
    Condition
      Error:
      ! The supplied statistic `stat = "diff in props"` is incompatible with the supplied hypothesis `null = "point"`.

---

    Code
      gss %>% specify(college ~ sex, success = "degree") %>% hypothesise(null = "point",
        p = 0.4) %>% generate(reps = 10, type = "draw") %>% calculate(stat = "diff in props",
        order = c("female", "male"))
    Condition
      Error:
      ! The supplied statistic `stat = "diff in props"` is incompatible with the supplied hypothesis `null = "point"`.

# response attribute has been set

    Code
      tibble::as_tibble(gss) %>% calculate(stat = "median")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

# variable chosen is of appropriate class (one var problems)

    Code
      calculate(gen_gss1, stat = "mean")
    Condition
      Error:
      ! A mean is not well-defined for a multinomial categorical response variable (partyid) and no explanatory variable.

---

    Code
      calculate(gen_gss_num, stat = "prop")
    Condition
      Error:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num, stat = "median")
    Condition
      Error:
      ! `"mu"` does not correspond to `stat = "median"`.

---

    Code
      calculate(gen_gss_num, stat = "sd")
    Condition
      Error:
      ! `"mu"` does not correspond to `stat = "sd"`.

---

    Code
      calculate(gen_gss_num2, stat = "prop")
    Condition
      Error:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num2, stat = "mean")
    Condition
      Error:
      ! `stat == "mean"` requires `"mu"` to be set in `hypothesize()`.

---

    Code
      calculate(gen_gss_num2, stat = "sd")
    Condition
      Error:
      ! `"med"` does not correspond to `stat = "sd"`.

---

    Code
      calculate(gen_gss_num3, stat = "prop")
    Condition
      Error:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num3, stat = "mean")
    Condition
      Error:
      ! `stat == "mean"` requires `"mu"` to be set in `hypothesize()`.

---

    Code
      calculate(gen_gss_num3, stat = "median")
    Condition
      Error:
      ! `stat == "median"` requires `"med"` to be set in `hypothesize()`.

# grouping (explanatory) variable is a factor (two var problems)

    Code
      calculate(gen_gss2, stat = "diff in means")
    Condition
      Error:
      ! A difference in means is not well-defined for a numeric response variable (hours) and a numeric explanatory variable (age).

---

    Code
      calculate(gen_gss2, stat = "diff in medians")
    Condition
      Error:
      ! A difference in medians is not well-defined for a numeric response variable (hours) and a numeric explanatory variable (age).

# grouping (explanatory) variable is numeric (two var problems)

    Code
      calculate(gen_gss2a, stat = "slope")
    Condition
      Error:
      ! The infer team has not implemented test statistics for the supplied variable types.

---

    Code
      calculate(gen_gss2a, stat = "t")
    Condition
      Error:
      ! The infer team has not implemented test statistics for the supplied variable types.

---

    Code
      calculate(gen_gss2a, stat = "diff in medians")
    Condition
      Error:
      ! The infer team has not implemented test statistics for the supplied variable types.

# response variable is a factor (two var problems)

    Code
      calculate(gen_gss3, stat = "Chisq")
    Condition
      Error:
      ! A chi-square statistic is not well-defined for a numeric response variable (hours) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "diff in props")
    Condition
      Error:
      ! A difference in proportions is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "ratio of props")
    Condition
      Error:
      ! A ratio of proportions is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "odds ratio")
    Condition
      Error:
      ! An odds ratio is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "t")
    Condition
      Error:
      ! A t statistic is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

# response variable is numeric (two var problems)

    Code
      calculate(gen_gss5, stat = "F")
    Condition
      Error:
      ! The infer team has not implemented test statistics for the supplied variable types.

# properties of tibble passed-in are correct

    Code
      calculate(gen_gss6)
    Condition
      Error:
      ! `stat` must be 'string', not 'character'.

# chi-square matches chisq.test value

    Code
      dat %>% specify(action ~ sex, success = "promote") %>% calculate(stat = "Chisq",
        order = c("male", "female"), correct = "boop")
    Condition
      Error in `dplyr::summarise()`:
      i In argument: `stat = chisq_indep(data)`.
      i In row 1.
      Caused by error in `correct && nrow(x) == 2L`:
      ! invalid 'x' type in 'x && y'

# `order` is working

    Code
      calculate(gen_gss_tbl10, stat = "diff in means", order = c(TRUE, FALSE))
    Condition
      Error:
      ! TRUE is not a level of the explanatory variable.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = "no degree")
    Condition
      Error:
      ! Only one level specified in `order`. Both levels need to be specified.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = c(NA, "no degree"))
    Condition
      Error:
      ! Only one level specified in `order`. Both levels need to be specified.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = c("no degree",
        "other"))
    Condition
      Error:
      ! other is not a level of the explanatory variable.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in means", order = c("no degree",
        "degree", "the last one"))
    Condition
      Error:
      ! `order` is expecting only two entries.

# NULL response gives error

    Code
      gss_tbl_improp %>% calculate(stat = "mean")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

# specify done before calculate

    Code
      calculate(gss_tbl_mean, stat = "mean")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

---

    Code
      calculate(gss_tbl_prop, stat = "prop")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

---

    Code
      calculate(gss_tbl_prop, stat = "count")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

# calculate errors out with multiple explanatory variables

    Code
      gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        calculate(stat = "t")
    Condition
      Error:
      ! Multiple explanatory variables are not supported in calculate(). When working with multiple explanatory variables, use fit() instead.

---

    Code
      gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        generate(reps = 3, type = "permute") %>% calculate(stat = "t")
    Condition
      Error:
      ! Multiple explanatory variables are not supported in calculate(). When working with multiple explanatory variables, use fit() instead.

