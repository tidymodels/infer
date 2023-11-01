# x is a tibble

    Code
      calculate(vec, stat = "mean")
    Condition
      Error in `calculate()`:
      ! `x` must be 'tibble', not 'integer'.

# calculate checks `stat` argument

    Code
      calculate(gss_tbl, stat = 3)
    Condition
      Error in `calculate()`:
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
      Error in `calculate()`:
      ! The supplied statistic `stat = "diff in props"` is incompatible with the supplied hypothesis `null = "point"`.

---

    Code
      gss %>% specify(college ~ sex, success = "degree") %>% hypothesise(null = "point",
        p = 0.4) %>% generate(reps = 10, type = "draw") %>% calculate(stat = "diff in props",
        order = c("female", "male"))
    Condition
      Error in `calculate()`:
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
      Error in `calculate()`:
      ! A mean is not well-defined for a multinomial categorical response variable (partyid) and no explanatory variable.

---

    Code
      calculate(gen_gss_num, stat = "prop")
    Condition
      Error in `calculate()`:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num, stat = "median")
    Condition
      Error in `calculate()`:
      ! `"mu"` does not correspond to `stat = "median"`.

---

    Code
      calculate(gen_gss_num, stat = "sd")
    Condition
      Error in `calculate()`:
      ! `"mu"` does not correspond to `stat = "sd"`.

---

    Code
      calculate(gen_gss_num2, stat = "prop")
    Condition
      Error in `calculate()`:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num2, stat = "mean")
    Condition
      Error in `calculate()`:
      ! `stat == "mean"` requires `"mu"` to be set in `hypothesize()`.

---

    Code
      calculate(gen_gss_num2, stat = "sd")
    Condition
      Error in `calculate()`:
      ! `"med"` does not correspond to `stat = "sd"`.

---

    Code
      calculate(gen_gss_num3, stat = "prop")
    Condition
      Error in `calculate()`:
      ! A proportion is not well-defined for a numeric response variable (hours) and no explanatory variable.

---

    Code
      calculate(gen_gss_num3, stat = "mean")
    Condition
      Error in `calculate()`:
      ! `stat == "mean"` requires `"mu"` to be set in `hypothesize()`.

---

    Code
      calculate(gen_gss_num3, stat = "median")
    Condition
      Error in `calculate()`:
      ! `stat == "median"` requires `"med"` to be set in `hypothesize()`.

# grouping (explanatory) variable is a factor (two var problems)

    Code
      calculate(gen_gss2, stat = "diff in means")
    Condition
      Error in `calculate()`:
      ! A difference in means is not well-defined for a numeric response variable (hours) and a numeric explanatory variable (age).

---

    Code
      calculate(gen_gss2, stat = "diff in medians")
    Condition
      Error in `calculate()`:
      ! A difference in medians is not well-defined for a numeric response variable (hours) and a numeric explanatory variable (age).

# grouping (explanatory) variable is numeric (two var problems)

    Code
      calculate(gen_gss2a, stat = "slope")
    Condition
      Error in `calculate()`:
      ! The infer team has not implemented test statistics for the supplied variable types.

---

    Code
      calculate(gen_gss2a, stat = "t")
    Condition
      Error in `calculate()`:
      ! The infer team has not implemented test statistics for the supplied variable types.

---

    Code
      calculate(gen_gss2a, stat = "diff in medians")
    Condition
      Error in `calculate()`:
      ! The infer team has not implemented test statistics for the supplied variable types.

# response variable is a factor (two var problems)

    Code
      calculate(gen_gss3, stat = "Chisq")
    Condition
      Error in `calculate()`:
      ! A chi-square statistic is not well-defined for a numeric response variable (hours) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "diff in props")
    Condition
      Error in `calculate()`:
      ! A difference in proportions is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "ratio of props")
    Condition
      Error in `calculate()`:
      ! A ratio of proportions is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "odds ratio")
    Condition
      Error in `calculate()`:
      ! An odds ratio is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      calculate(gen_gss4, stat = "t")
    Condition
      Error in `calculate()`:
      ! A t statistic is not well-defined for a dichotomous categorical response variable (sex) and a multinomial categorical explanatory variable (partyid).

---

    Code
      res_ <- calculate(gen_gss4a, stat = "z")
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "male" - "female", or divided in the order "male" / "female" for ratio-based statistics. To specify this order yourself, supply `order = c("male", "female")` to the calculate() function.

# response variable is numeric (two var problems)

    Code
      calculate(gen_gss5, stat = "F")
    Condition
      Error in `calculate()`:
      ! The infer team has not implemented test statistics for the supplied variable types.

# two sample mean-type problems are working

    Code
      res_ <- calculate(gen_gss5a, stat = "diff in means")
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "no degree" - "degree", or divided in the order "no degree" / "degree" for ratio-based statistics. To specify this order yourself, supply `order = c("no degree", "degree")` to the calculate() function.

---

    Code
      res_ <- calculate(gen_gss5a, stat = "t")
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "no degree" - "degree", or divided in the order "no degree" / "degree" for ratio-based statistics. To specify this order yourself, supply `order = c("no degree", "degree")` to the calculate() function.

# properties of tibble passed-in are correct

    Code
      calculate(gen_gss6)
    Condition
      Error in `calculate()`:
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

# chi-square works with factors with unused levels

    Code
      out <- test_tbl %>% specify(y ~ x) %>% calculate(stat = "Chisq") %>% pull()
    Message
      Dropping unused factor levels d from the supplied explanatory variable 'x'.

---

    Code
      out <- test_tbl %>% specify(y ~ x) %>% calculate(stat = "Chisq") %>% pull()
    Message
      Dropping unused factor levels g from the supplied response variable 'y'.

# `order` is working

    Code
      calculate(gen_gss_tbl10, stat = "diff in means", order = c(TRUE, FALSE))
    Condition
      Error in `calculate()`:
      ! TRUE is not a level of the explanatory variable.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = "no degree")
    Condition
      Error in `calculate()`:
      ! Only one level specified in `order`. Both levels need to be specified.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = c(NA, "no degree"))
    Condition
      Error in `calculate()`:
      ! Only one level specified in `order`. Both levels need to be specified.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in medians", order = c("no degree",
        "other"))
    Condition
      Error in `calculate()`:
      ! other is not a level of the explanatory variable.

---

    Code
      calculate(gen_gss_tbl11, stat = "diff in means", order = c("no degree",
        "degree", "the last one"))
    Condition
      Error in `calculate()`:
      ! `order` is expecting only two entries.

---

    Code
      res_ <- calculate(gen_gss_tbl11, stat = "diff in means")
    Condition
      Warning:
      The statistic is based on a difference or ratio; by default, for difference-based statistics, the explanatory variable is subtracted in the order "no degree" - "degree", or divided in the order "no degree" / "degree" for ratio-based statistics. To specify this order yourself, supply `order = c("no degree", "degree")` to the calculate() function.

# NULL response gives error

    Code
      gss_tbl_improp %>% calculate(stat = "mean")
    Condition
      Error in `dplyr::filter()`:
      i In argument: `resp == response_type & exp == explanatory_type`.
      Caused by error:
      ! `..1` must be of size 10 or 1, not size 0.

# order being given when not needed gives warning

    Code
      res_ <- calculate(gen_gss_tbl15, stat = "Chisq", order = c("dem", "ind"))
    Condition
      Warning:
      Statistic is not based on a difference or ratio; the `order` argument will be ignored. Check `calculate()` (`?infer::calculate()`) for details.

# specify() %>% calculate() works

    Code
      res_ <- gss_tbl %>% specify(hours ~ NULL) %>% hypothesize(null = "point", mu = 4) %>%
        calculate(stat = "mean")
    Message
      Message: The point null hypothesis `mu = 4` does not inform calculation of the observed statistic (a mean) and will be ignored.

---

    Code
      res_ <- gss_tbl %>% specify(partyid ~ NULL) %>% calculate(stat = "Chisq")
    Condition
      Warning:
      A chi-square statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null values: `p = c(dem = 0.333333333333333, ind = 0.333333333333333, rep = 0.333333333333333)`.

# One sample t hypothesis test is working

    Code
      res_ <- gss_tbl %>% specify(hours ~ NULL) %>% hypothesize(null = "point", mu = 1) %>%
        generate(reps = 10) %>% calculate(stat = "t")
    Message
      Setting `type = "bootstrap"` in `generate()`.

---

    Code
      res_ <- gss_tbl %>% specify(response = hours) %>% calculate(stat = "t")
    Condition
      Warning:
      A t statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null value: `mu = 0`.

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

# chisq GoF has params specified for observed stat

    Code
      res_ <- calculate(no_params, stat = "Chisq")
    Condition
      Warning:
      A chi-square statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null values: `p = c(dem = 0.333333333333333, ind = 0.333333333333333, rep = 0.333333333333333)`.

# One sample t bootstrap is working

    Code
      res_ <- gss_tbl %>% specify(hours ~ NULL) %>% generate(reps = 10, type = "bootstrap") %>%
        calculate(stat = "t")
    Condition
      Warning:
      A t statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null value: `mu = 0`.

# calculate warns informatively with insufficient null

    Code
      res_ <- gss %>% specify(response = sex, success = "female") %>% calculate(stat = "z")
    Condition
      Warning:
      A z statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null value: `p = .5`.

---

    Code
      res_ <- gss %>% specify(hours ~ NULL) %>% calculate(stat = "t")
    Condition
      Warning:
      A t statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null value: `mu = 0`.

---

    Code
      res_ <- gss %>% specify(response = partyid) %>% calculate(stat = "Chisq")
    Message
      Dropping unused factor levels DK from the supplied response variable 'partyid'.
    Condition
      Warning:
      A chi-square statistic requires a null hypothesis to calculate the observed statistic.
      Output assumes the following null values: `p = c(dem = 0.2, ind = 0.2, rep = 0.2, other = 0.2, DK = 0.2)`.

# calculate messages informatively with excessive null

    Code
      res_ <- gss %>% specify(hours ~ NULL) %>% hypothesize(null = "point", mu = 40) %>%
        calculate(stat = "mean")
    Message
      Message: The point null hypothesis `mu = 40` does not inform calculation of the observed statistic (a mean) and will be ignored.

---

    Code
      res_ <- gss %>% specify(hours ~ NULL) %>% hypothesize(null = "point", sigma = 10) %>%
        calculate(stat = "sd")
    Message
      Message: The point null hypothesis `sigma = 10` does not inform calculation of the observed statistic (a standard deviation) and will be ignored.

---

    Code
      res_ <- gss %>% specify(hours ~ college) %>% hypothesize(null = "independence") %>%
        calculate("diff in means", order = c("no degree", "degree"))
    Message
      Message: The independence null hypothesis does not inform calculation of the observed statistic (a difference in means) and will be ignored.

# calculate errors out with multiple explanatory variables

    Code
      gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        calculate(stat = "t")
    Condition
      Error in `calculate()`:
      ! Multiple explanatory variables are not supported in `calculate()`.
      i When working with multiple explanatory variables, use `fit()` (`?infer::fit.infer()`) instead.

---

    Code
      gss %>% specify(hours ~ age + college) %>% hypothesize(null = "independence") %>%
        generate(reps = 3, type = "permute") %>% calculate(stat = "t")
    Condition
      Error in `calculate()`:
      ! Multiple explanatory variables are not supported in `calculate()`.
      i When working with multiple explanatory variables, use `fit()` (`?infer::fit.infer()`) instead.

