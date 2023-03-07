# calculate arguments
test_that("x is a tibble", {
  vec <- 1:10
  expect_error(calculate(vec, stat = "mean"))
})

test_that("calculate checks `stat` argument", {
  # stat is a string
  expect_error(calculate(gss_tbl, stat = 3))

  # stat is one of the implemented options with informative error
  gen_gss_slope <- gss_tbl %>%
    specify(hours ~ age) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")

  expect_error(
    calculate(gen_gss_slope, stat = "slopee"),
    '`stat` must be one of.*Did you mean "slope"'
  )
  expect_error(
    calculate(gen_gss_slope, stat = "stdev"),
    "`stat` must be one of"
  )
  expect_error(
    calculate(gen_gss_slope, stat = "stat"),
    "`stat` must be one of"
  )
  expect_error(
    calculate(gen_gss_slope, stat = "chi sq"),
    '`stat` must be one of.*Did you mean "Chisq"'
  )

  # stat can be one of the allowed aliases
  chisq_df <- gss %>% specify(formula = finrela ~ sex)
  expect_equal(
    calculate(chisq_df, stat = "Chisq")[["stat"]],
    calculate(chisq_df, stat = "chisq")[["stat"]]
  )

  f_df <- gss %>% specify(age ~ partyid)
  expect_equal(
    calculate(f_df, stat = "F")[["stat"]],
    calculate(f_df, stat = "f")[["stat"]]
  )
})

test_that("errors informatively with incompatible stat vs hypothesis", {
  expect_error(
    gss %>%
      specify(college ~ sex, success = "degree") %>%
      hypothesise(null = "point", p = .40) %>%
      calculate(stat = "diff in props", order = c("female", "male")),
    'statistic \\`stat = "diff in props"\\` is incompatible'
  )

  expect_error(
    gss %>%
      specify(college ~ sex, success = "degree") %>%
      hypothesise(null = "point", p = .40) %>%
      generate(reps = 10, type = "draw") %>%
      calculate(stat = "diff in props", order = c("female", "male")),
    'statistic \\`stat = "diff in props"\\` is incompatible'
  )

  expect_silent(
    gss %>%
      specify(hours ~ college) %>%
      hypothesize(null = "point", mu = 40) %>%
      calculate(stat = "t", order = c("degree", "no degree"))
  )

  expect_silent(
    gss %>%
      specify(response = finrela) %>%
      hypothesize(null = "point",
                  p = c("far below average" = 1/6,
                        "below average" = 1/6,
                        "average" = 1/6,
                        "above average" = 1/6,
                        "far above average" = 1/6,
                        "DK" = 1/6)) %>%
      calculate(stat = "Chisq")
  )
})

test_that("response attribute has been set", {
  expect_error(
    tibble::as_tibble(gss) %>% calculate(stat = "median")
  )
})

test_that("variable chosen is of appropriate class (one var problems)", {
  # One sample chisq example
  gen_gss1 <- gss_tbl %>%
    specify(partyid ~ NULL) %>%
    hypothesize(
      null = "point",
      p = c("dem" = .5, "rep" = .25, "ind" = .25)
    ) %>%
    generate(reps = 10, type = "draw")
  expect_error(calculate(gen_gss1, stat = "mean"))

  # One mean example
  gen_gss_num <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    hypothesize(null = "point", mu = 40) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_gss_num, stat = "prop"))
  expect_silent(calculate(gen_gss_num, stat = "mean"))
  expect_error(calculate(gen_gss_num, stat = "median"))
  expect_error(calculate(gen_gss_num, stat = "sd"))

  gen_gss_num2 <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    hypothesize(null = "point", med = 40) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_gss_num2, stat = "prop"))
  expect_error(calculate(gen_gss_num2, stat = "mean"))
  expect_silent(calculate(gen_gss_num2, stat = "median"))
  expect_error(calculate(gen_gss_num2, stat = "sd"))

  gen_gss_num3 <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    hypothesize(null = "point", sigma = 0.6) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_gss_num3, stat = "prop"))
  expect_error(calculate(gen_gss_num3, stat = "mean"))
  expect_error(calculate(gen_gss_num3, stat = "median"))
  expect_silent(calculate(gen_gss_num3, stat = "sd"))
})

test_that("grouping (explanatory) variable is a factor (two var problems)", {
  gen_gss2 <- gss_tbl %>%
    specify(hours ~ age) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_gss2, stat = "diff in means"))
  expect_error(calculate(gen_gss2, stat = "diff in medians"))
  # Since shifts to "Slope with t"
  ## Not implemented
  # expect_silent(calculate(gen_gss2, stat = "t"))
})

test_that("grouping (explanatory) variable is numeric (two var problems)", {
  gen_gss2a <- gss_tbl %>%
    specify(partyid ~ hours) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_gss2a, stat = "slope"))
  # Since shifts to "Slope with t"
  expect_error(calculate(gen_gss2a, stat = "t"))
  expect_error(calculate(gen_gss2a, stat = "diff in medians"))
})

test_that("response variable is a factor (two var problems)", {
  gen_gss3 <- gss_tbl %>%
    specify(hours ~ partyid) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_gss3, stat = "Chisq"))

  # explanatory has more than 2 levels
  gen_gss4 <- gss_tbl %>%
    specify(sex ~ partyid, success = "female") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_gss4, stat = "diff in props"))
  expect_error(calculate(gen_gss4, stat = "ratio of props"))
  expect_error(calculate(gen_gss4, stat = "odds ratio"))

  expect_error(calculate(gen_gss4, stat = "t"))

  # Check successful diff in props
  gen_gss4a <- gss_tbl %>%
    specify(college ~ sex, success = "no degree") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(
    calculate(gen_gss4a, stat = "diff in props", order = c("female", "male"))
  )
  expect_silent(
    calculate(gen_gss4a, stat = "ratio of props", order = c("female", "male"))
  )
  expect_silent(
    calculate(gen_gss4a, stat = "odds ratio", order = c("female", "male"))
  )
  expect_silent(
    calculate(gen_gss4a, stat = "z", order = c("female", "male"))
  )
  expect_warning(calculate(gen_gss4a, stat = "z"))
})

gen_gss5 <- gss_tbl %>%
  specify(partyid ~ hours) %>%
  generate(reps = 10, type = "bootstrap")

test_that("response variable is numeric (two var problems)", {
  expect_error(calculate(gen_gss5, stat = "F"))
})

test_that("two sample mean-type problems are working", {
  gen_gss5a <- gss_tbl %>%
    specify(hours ~ college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_warning(calculate(gen_gss5a, stat = "diff in means"))
  expect_silent(
    calculate(gen_gss5a,
      stat = "diff in means",
      order = c("no degree", "degree")
    )
  )
  expect_warning(calculate(gen_gss5a, stat = "t"))
  expect_silent(calculate(gen_gss5a,
    stat = "t",
    order = c("no degree", "degree")
  ))
})

test_that("properties of tibble passed-in are correct", {
  expect_is(gen_gss5, "grouped_df")
  expect_equal(ncol(gen_gss5), 3)

  gen_gss6 <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    generate(reps = 10)
  expect_equal(ncol(gen_gss6), 2)
  expect_error(calculate(gen_gss6))
})

test_that("order is working for diff in means", {
  gen_gss7 <- gss_tbl %>%
    specify(hours ~ college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_equal(
    nrow(calculate(gen_gss7,
      stat = "diff in means",
      order = c("no degree", "degree")
    )),
    10
  )
  expect_equal(
    ncol(calculate(gen_gss7,
      stat = "diff in means",
      order = c("no degree", "degree")
    )),
    2
  )
})

test_that("chi-square matches chisq.test value", {
  gen_gss8 <- gss_tbl %>%
    specify(sex ~ partyid, success = "female") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  infer_way <- calculate(gen_gss8, stat = "Chisq")
  # chisq.test way
  suppressWarnings(
    trad_way <- gen_gss8 %>%
      dplyr::group_by(replicate) %>%
      dplyr::do(broom::tidy(
        stats::chisq.test(table(.$sex, .$partyid))
      )) %>%
      dplyr::ungroup() %>%
      dplyr::select(replicate, stat = statistic)
  )
  # Equal not including attributes
  expect_equivalent(infer_way, trad_way)

  gen_gss9 <- gss_tbl %>%
    specify(partyid ~ NULL) %>%
    hypothesize(
      null = "point",
      p = c("dem" = 1 / 3, "rep" = 1 / 3, "ind" = 1 / 3)
    ) %>%
    generate(reps = 10, type = "draw")
  infer_way <- calculate(gen_gss9, stat = "Chisq")
  # chisq.test way
  trad_way <- gen_gss9 %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(
      stats::chisq.test(table(.$partyid))
    )) %>%
    dplyr::select(replicate, stat = statistic)
  expect_equivalent(infer_way, trad_way)

  gen_gss9a <- gss_tbl %>%
    specify(partyid ~ NULL) %>%
    hypothesize(
      null = "point",
      p = c("dem" = 0.8, "rep" = 0.1, "ind" = 0.1)
    ) %>%
    generate(reps = 10, type = "draw")
  infer_way <- calculate(gen_gss9a, stat = "Chisq")
  # chisq.test way
  trad_way <- gen_gss9a %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(
      stats::chisq.test(table(.$partyid), p = c(0.8, 0.1, 0.1))
    )) %>%
    dplyr::select(replicate, stat = statistic)
  expect_equivalent(infer_way, trad_way)

  # check that dots are passed correctly
  dat <- data.frame(
     action = c(rep(x = "promote", times = 32),
                rep(x = "hold file", times = 12),
                rep(x = "promote", times =  19),
                rep(x = "hold file", times =  30)),
     sex = c(rep(x = "male", times =  44),
             rep(x = "female", times = 49)))


  promote_f <- dat %>%
     specify(action ~ sex, success = "promote") %>%
     calculate(stat = "Chisq", order = c("male", "female"), correct = FALSE)

  promote_t <- dat %>%
     specify(action ~ sex, success = "promote") %>%
     calculate(stat = "Chisq", order = c("male", "female"), correct = TRUE)

  expect_false(promote_f$stat == promote_t$stat)

  expect_error(
    dat %>%
     specify(action ~ sex, success = "promote") %>%
     calculate(stat = "Chisq", order = c("male", "female"), correct = "boop")
  )
})

test_that("chi-square works with factors with unused levels", {
  test_tbl <- tibble(
    x = factor(c("a", "b", "c"), levels = c("a", "b", "c", "d")),
    y = factor(c("e", "e", "f"))
  )

  # Unused levels in explanatory variable
  expect_message(
    out <- test_tbl %>%
      specify(y ~ x) %>%
      calculate(stat = "Chisq") %>%
      pull(),
    "Dropping unused factor levels.*explanatory"
  )
  expect_true(!is.na(out))

  # Unused levels in response variable
  test_tbl[["x"]] <- factor(test_tbl[["x"]])
  levels(test_tbl[["y"]]) <- c("e", "f", "g")
  expect_message(
    out <- test_tbl %>%
      specify(y ~ x) %>%
      calculate(stat = "Chisq") %>%
      pull(),
    "Dropping unused factor levels.*response"
  )
  expect_true(!is.na(out))
})

test_that("`order` is working", {
  gen_gss_tbl10 <- gss_tbl %>%
    specify(hours ~ college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(
    calculate(gen_gss_tbl10, stat = "diff in means", order = c(TRUE, FALSE))
  )

  gen_gss_tbl11 <- gss_tbl %>%
    specify(hours ~ college) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(
    calculate(gen_gss_tbl11,
      stat = "diff in medians",
      order = "no degree"
    )
  )
  expect_error(
    calculate(gen_gss_tbl11,
      stat = "diff in medians",
      order = c(NA, "no degree")
    )
  )
  expect_error(
    calculate(gen_gss_tbl11,
      stat = "diff in medians",
      order = c("no degree", "other")
    )
  )
  expect_silent(
    calculate(gen_gss_tbl11,
      stat = "diff in medians",
      order = c("no degree", "degree")
    )
  )
  expect_error(
    calculate(gen_gss_tbl11,
      stat = "diff in means",
      order = c("no degree", "degree", "the last one")
    )
  )
  # order not given
  expect_warning(
    calculate(gen_gss_tbl11, stat = "diff in means"),
    "The statistic is based on a difference or ratio"
  )
})

gen_gss_tbl12 <- gss_tbl %>%
  specify(college ~ NULL, success = "no degree") %>%
  hypothesize(null = "point", p = 0.3) %>%
  generate(reps = 10, type = "draw")

test_that('success is working for stat = "prop"', {
  expect_silent(gen_gss_tbl12 %>% calculate(stat = "prop"))
  expect_silent(gen_gss_tbl12 %>% calculate(stat = "z"))
})

test_that("NULL response gives error", {
  gss_tbl_improp <- tibble::as_tibble(gss_tbl) %>%
    dplyr::select(hours, age)

  expect_error(gss_tbl_improp %>% calculate(stat = "mean"))
})

test_that("Permute F test works", {
  gen_gss_tbl13 <- gss_tbl %>%
    specify(hours ~ partyid) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_gss_tbl13, stat = "F"))
})

test_that("Permute slope/correlation test works", {
  gen_gss_tbl14 <- gss_tbl %>%
    specify(hours ~ age) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_gss_tbl14, stat = "slope"))
  expect_silent(calculate(gen_gss_tbl14, stat = "correlation"))
})

test_that("order being given when not needed gives warning", {
  gen_gss_tbl15 <- gss_tbl %>%
    specify(college ~ partyid, success = "no degree") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_warning(
    calculate(gen_gss_tbl15, stat = "Chisq", order = c("dem", "ind"))
  )
})

## Breaks oldrel build. Commented out for now.
# test_that("warning given if calculate without generate", {
#   expect_warning(
#     gss_tbl %>%
#       specify(partyid ~ NULL) %>%
#       hypothesize(
#         null = "point",
#         p = c("dem" = 0.4, "rep" = 0.4, "ind" = 0.2)
#       ) %>%
#       # generate(reps = 10, type = "draw") %>%
#       calculate(stat = "Chisq")
#   )
# })

test_that("specify() %>% calculate() works", {
  expect_silent(
    gss_tbl %>% specify(hours ~ NULL) %>% calculate(stat = "mean")
  )
  expect_message(
    gss_tbl %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 4) %>%
      calculate(stat = "mean")
  )

  expect_warning(
    gss_tbl %>% specify(partyid ~ NULL) %>% calculate(stat = "Chisq")
  )
})

test_that("One sample t hypothesis test is working", {
  expect_message(
    gss_tbl %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 1) %>%
      generate(reps = 10) %>%
      calculate(stat = "t")
  )

  expect_warning(
    gss_tbl %>%
      specify(response = hours) %>%
      calculate(stat = "t"),
    "A t statistic requires"
  )

  gss_tbl %>%
    specify(response = hours) %>%
    calculate(stat = "t", mu = 1)
})

test_that("specify done before calculate", {
  gss_tbl_mean <- gss_tbl %>%
    dplyr::select(stat = hours)
  expect_error(calculate(gss_tbl_mean, stat = "mean"))

  gss_tbl_prop <- gss_tbl %>% dplyr::select(college)
  attr(gss_tbl_prop, "response") <- "college"
  expect_error(calculate(gss_tbl_prop, stat = "prop"))
  expect_error(calculate(gss_tbl_prop, stat = "count"))
})

test_that("chisq GoF has params specified for observed stat", {
  no_params <- gss_tbl %>% specify(response = partyid)
  expect_warning(calculate(no_params, stat = "Chisq"))

  params <- gss_tbl %>%
    specify(response = partyid) %>%
    hypothesize(
      null = "point",
      p = c("dem" = .5, "rep" = .25, "ind" = .25)
    )
  expect_silent(calculate(params, stat = "Chisq"))
})

test_that("One sample t bootstrap is working", {
  expect_warning(
    gss_tbl %>%
      specify(hours ~ NULL) %>%
      generate(reps = 10, type = "bootstrap") %>%
      calculate(stat = "t")
  )
})

test_that("calculate doesn't depend on order of `p` (#122)", {
  calc_chisq <- function(p) {
    set.seed(111)

    gss_tbl %>%
      specify(partyid ~ NULL) %>%
      hypothesize(null = "point", p = p) %>%
      generate(reps = 500, type = "draw") %>%
      calculate("Chisq") %>%
      get_p_value(obs_stat = 5, direction = "right")
  }

  expect_equal(
    calc_chisq(c("rep" = 0.25, "dem" = 0.5, "ind" = 0.25)),
    calc_chisq(c("ind" = 0.25, "rep" = 0.25, "dem" = 0.5)),
    tolerance = eps
  )
})

test_that("calc_impl_one_f works", {
  expect_true(is.function(calc_impl_one_f(mean)))
})

test_that("calc_impl_diff_f works", {
  expect_true(is.function(calc_impl_diff_f(mean)))
})

test_that("calc_impl.sum works", {
  expect_equal(
    gss_tbl %>%
      specify(hours ~ NULL) %>%
      calculate(stat = "sum") %>%
      `[[`(1),
    sum(gss_tbl$hours),
    tolerance = eps
  )

  gen_gss_tbl16 <- gss_tbl %>%
    specify(hours ~ NULL) %>%
    generate(10)

  expect_equivalent(
    gen_gss_tbl16 %>% calculate(stat = "sum"),
    gen_gss_tbl16 %>% dplyr::summarise(stat = sum(hours))
  )
})

test_that("calc_impl_success_f works", {
  expect_true(
    is.function(calc_impl_success_f(
      f = function(response, success, ...) {
        mean(response == success, ...)
      },
      output_name = "proportion"
    ))
  )
})

test_that("calc_impl.count works", {
  expect_equal(
    gss_tbl %>%
      specify(college ~ NULL, success = "no degree") %>%
      calculate(stat = "count") %>%
      `[[`(1),
    sum(gss_tbl$college == "no degree"),
    tolerance = eps
  )

  expect_equivalent(
    gen_gss_tbl12 %>% calculate(stat = "count"),
    gen_gss_tbl12 %>% dplyr::summarise(stat = sum(college == "no degree"))
  )
})


gss_biased <- gss_tbl %>%
  dplyr::filter(!(sex == "male" & college == "no degree" & age < 40))

gss_tbl <- table(gss_biased$sex, gss_biased$college)

test_that("calc_impl.odds_ratio works", {
  base_odds_ratio <- {
    (gss_tbl [1, 1] * gss_tbl [2, 2]) /
      (gss_tbl [1, 2] * gss_tbl [2, 1])
  }

  expect_equal(
    gss_biased %>%
      specify(college ~ sex, success = "degree") %>%
      calculate(stat = "odds ratio", order = c("female", "male")) %>%
      dplyr::pull(),
    expected = base_odds_ratio,
    tolerance = eps
  )
})

test_that("calc_impl.ratio_of_props works", {
  base_ratio_of_props <- {
    (gss_tbl [1, 2] / sum(gss_tbl [1, ])) /
      (gss_tbl [2, 2] / sum(gss_tbl [2, ]))
  }

  expect_equal(
    gss_biased %>%
      specify(college ~ sex, success = "degree") %>%
      calculate(stat = "ratio of props", order = c("male", "female")) %>%
      dplyr::pull(),
    expected = base_ratio_of_props,
    tolerance = eps
  )
})

test_that("calc_impl.ratio_of_means works", {
   base_ratio_of_means <- {
      mean(gss$age[gss$college == "degree"]) /
      mean(gss$age[gss$college == "no degree"])
   }

   expect_equal(
      gss %>%
         specify(age ~ college) %>%
         calculate("ratio of means", order = c("degree", "no degree")) %>%
         dplyr::pull(),
      expected = base_ratio_of_means,
      tolerance = eps
   )
})

test_that("calc_impl.z works for one sample proportions", {
  infer_obs_stat <- gss %>%
    specify(response = sex, success = "female") %>%
    hypothesize(null = "point", p = .5) %>%
    calculate(stat = "z") %>%
    dplyr::pull()

  base_obs_stat <-
    (mean(gss$sex == "female") - .5) /
    sqrt(.5^2 / nrow(gss))

  expect_equal(infer_obs_stat, base_obs_stat, tolerance = eps)
})

test_that("calculate warns informatively with insufficient null", {
  expect_warning(
    gss %>%
      specify(response = sex, success = "female") %>%
      calculate(stat = "z"),
    "following null value: `p = .5`"
  )

  expect_warning(
    gss %>%
      specify(hours ~ NULL) %>%
      calculate(stat = "t"),
    "following null value: `mu = 0`"
  )

  expect_warning(
    gss %>%
      specify(response = partyid) %>%
      calculate(stat = "Chisq"),
    "the following null values: `p = c(dem = 0.2",
    fixed = TRUE
  )
})

test_that("calculate messages informatively with excessive null", {
  expect_message(
    gss %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 40) %>%
      calculate(stat = "mean"),
    "point null hypothesis `mu = 40` does not inform calculation"
  )

  expect_message(
    gss %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", sigma = 10) %>%
      calculate(stat = "sd"),
    "point null hypothesis `sigma = 10` does not inform calculation"
  )

  expect_message(
    gss %>%
      specify(hours ~ college) %>%
      hypothesize(null = "independence") %>%
      calculate("diff in means", order = c("no degree", "degree")),
    "independence null hypothesis does not inform calculation"
  )
})

test_that("calculate can handle variables named x", {
  expect_silent({
    t_0 <- data.frame(x = 1:10) %>%
      specify(response = x) %>%
      hypothesise(null = "point", mu = 0) %>%
      calculate(stat = "t")
  })

  expect_silent({
    t_1 <- data.frame(sample = 1:10) %>%
      specify(response = sample) %>%
      hypothesise(null = "point", mu = 0) %>%
      calculate(stat = "t")
  })

  expect_equal(
    unname(t_0$stat),
    unname(t_1$stat),
    tolerance = .001
  )
})

test_that("calculate errors out with multiple explanatory variables", {
  expect_error(
    gss %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      calculate(stat = "t"),
    "Multiple explanatory variables.*in calculate"
  )

  expect_error(
    gss %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 3, type = "permute") %>%
      calculate(stat = "t"),
    "Multiple explanatory variables.*in calculate"
  )
})

test_that("reported standard errors are correct", {
  # mean ---------------------------------------------------------------------
  x_bar <- gss %>%
    specify(response = hours) %>%
    calculate(stat = "mean")

  expect_equal(
    attr(x_bar, "se"),
    stats::sd(gss$hours)/sqrt(nrow(gss)),
    tolerance = 1e-6
  )

  # prop ---------------------------------------------------------------------
  p_hat <- gss %>%
    specify(response = sex, success = "female") %>%
    calculate(stat = "prop")

  expect_equal(
    attr(p_hat, "se"),
    sqrt(
      (mean(gss$sex == "female") * (1 - mean(gss$sex == "female"))) / nrow(gss)
    ),
    tolerance = 1e-6
  )

  # diff in means ------------------------------------------------------------
  diff_bar <- gss %>%
    specify(hours ~ college) %>%
    calculate(stat = "diff in means", order = c("no degree", "degree"))

  expect_equal(
    attr(diff_bar, "se"),
    sqrt(
      (stats::sd(gss$hours[gss$college == "degree"]) /
         sqrt(nrow(gss[gss$college == "degree",])))^2 +
      (stats::sd(gss$hours[gss$college == "no degree"]) /
         sqrt(nrow(gss[gss$college == "no degree",])))^2
    ),
    tolerance = 1e-6
  )

  # diff in props ------------------------------------------------------------
  diff_hat <- gss %>%
    specify(sex ~ college, success = "female") %>%
    calculate(stat = "diff in props", order = c("no degree", "degree"))

  expect_equal(
    attr(diff_hat, "se"),
    sqrt(
      abs((mean(gss[gss$college == "degree",]$sex == "female") *
          (1 - mean(gss[gss$college == "degree",]$sex == "female"))) /
          nrow(gss[gss$college == "degree",])) +
      abs((mean(gss[gss$college == "no degree",]$sex == "female") *
          (1 - mean(gss[gss$college == "no degree",]$sex == "female"))) /
          nrow(gss[gss$college == "no degree",]))
    ),
    tolerance = 1e-6
  )

  # ratio of means ------------------------------------------------------------
  # this stat shares machinery with others that report se, so make
  # sure that we don't
  rat_hat <- gss %>%
     specify(hours ~ college) %>%
     calculate(stat = "ratio of means", order = c("no degree", "degree"))

  expect_null(attr(rat_hat, "se"))
})











