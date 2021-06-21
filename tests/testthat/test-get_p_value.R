context("get_p_value")

set.seed(2018)
test_df <- gss_calc[1:20,]
test_df$stat <- sample(c(
  -5, -4, -4, -4, -1, -0.5, rep(0, 6), 1, 1, 3.999, 4, 4, 4.001, 5, 5
))

test_that("direction is appropriate", {
  expect_error(test_df %>% get_p_value(obs_stat = 0.5, direction = "righ"))
})

test_that("get_p_value works", {
  expect_equal(get_p_value(test_df, 4, "right")[[1]][1], 5/20, tolerance = eps)
  expect_equal(get_p_value(test_df, 4, "left")[[1]][1], 17/20, tolerance = eps)
  expect_equal(get_p_value(test_df, 4, "both")[[1]][1], 10/20, tolerance = eps)
  
  expect_equal(get_p_value(test_df, 0, "right")[[1]][1], 14/20, tolerance = eps)
  expect_equal(get_p_value(test_df, 0, "left")[[1]][1], 12/20, tolerance = eps)
  # This is also a check for not returning value more than 1
  expect_equal(get_p_value(test_df, 0, "both")[[1]][1], 1, tolerance = eps)
  
  expect_equal(get_p_value(test_df, -3.999, "right")[[1]][1], 16/20, tolerance = eps)
  expect_equal(get_p_value(test_df, -3.999, "left")[[1]][1], 4/20, tolerance = eps)
  expect_equal(get_p_value(test_df, -3.999, "both")[[1]][1], 8/20, tolerance = eps)
  
  expect_equal(
    get_p_value(test_df, 4, "greater"), 
    get_p_value(test_df, 4, "right"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "less"),
    get_p_value(test_df, 4, "left"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two_sided"), 
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two-sided"), 
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two sided"), 
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two.sided"), 
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
})

test_that("theoretical p-value not supported error", {
  obs_F <- gss_tbl %>% 
    specify(hours ~ partyid) %>%
    calculate(stat = "F") 
  expect_error(
    gss_tbl %>% 
      specify(hours ~ partyid) %>%
      hypothesize(null = "independence") %>% 
      calculate(stat = "F") %>% 
      get_p_value(obs_stat = obs_F, direction = "right")
  )
})

test_that("get_p_value warns in case of zero p-value", {
  expect_warning(
    get_p_value(gss_calc, obs_stat = -10, direction = "left"),
    "be cautious"
  )
})

test_that("get_p_value throws error in case of `NaN` stat", {
  gss_calc$stat[1] <- NaN
  expect_error(
    get_p_value(gss_calc, 0, "both"),
    "1 calculated statistic was `NaN`.*not well-defined"
  )
  
  gss_calc$stat[2] <- NaN
  expect_error(
    get_p_value(gss_calc, 0, "both"),
    "2 calculated statistics were `NaN`.*not well-defined"
  )
  
  # In the case that _all_ values are NaN, error should have different text
  gss_calc$stat <- NaN
  expect_error(get_p_value(gss_calc, 0, "both"), "All calculated stat")
})

test_that("get_p_value can handle fitted objects", {
  set.seed(1)
  
  null_fits <- gss[1:50,] %>%
    specify(hours ~ age + college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute") %>%
    fit()
  
  obs_fit <- gss[1:50,] %>%
    specify(hours ~ age + college) %>%
    fit()
  
  expect_equivalent(
    get_p_value(null_fits, obs_fit, "both"),
    structure(
      list(term = c("age", "collegedegree", "intercept"), 
           p_value = c(0.6, 0.4, 0.6)), 
      row.names = c(NA, -3L), 
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
  
  # errors out when it ought to
  obs_fit_2 <- gss[1:50,] %>%
    specify(hours ~ age) %>%
    fit()
  
  expect_error(
    get_p_value(null_fits, obs_fit_2, "both"),
    "explanatory variables.*are not the same used"
  )
  
  obs_fit_3 <- gss[1:50,] %>%
    specify(year ~ age + college) %>%
    fit()
  
  expect_error(
    get_p_value(null_fits, obs_fit_3, "both"),
    "response variable.*\\(hours\\) is not the same.*observed fit \\(year\\)."
  )
  
  set.seed(1)
  
  null_fits_4 <- gss[1:50,] %>%
    specify(hours ~ age) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute") %>%
    fit()
  
  obs_fit_4 <- gss[1:50,] %>%
    specify(hours ~ age) %>%
    fit()
  
  obs_fit_4
  
  expect_equivalent(
    get_p_value(null_fits_4, obs_fit_4, "both"),
    structure(
      list(
        term = c("age", "intercept"), 
        p_value = c(0.6, 0.6)), 
      row.names = c(NA, -2L), 
      class = c("tbl_df", "tbl", "data.frame")
    )
  )
  
  expect_equal(ncol(null_fits_4), ncol(obs_fit_4) + 1)
  expect_equal(nrow(null_fits_4), nrow(obs_fit_4) * 10)
  
  expect_equal(ncol(obs_fit_4), ncol(obs_fit))
  expect_equal(nrow(obs_fit_4), nrow(obs_fit) - 1)
  
  expect_true(is_fitted(obs_fit))
  expect_true(is_fitted(obs_fit_2))
  expect_true(is_fitted(obs_fit_3))
  expect_true(is_fitted(obs_fit_4))
  
  expect_true(is_fitted(null_fits))
  expect_true(is_fitted(null_fits_4))
})

test_that("get_p_value can handle bad args with fitted objects", {
  set.seed(1)
  
  null_fits <- gss[1:50,] %>%
    specify(hours ~ age + college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute") %>%
    fit()
  
  obs_fit <- gss[1:50,] %>%
    specify(hours ~ age + college) %>%
    fit()
  
  expect_error(
    get_p_value(null_fits, "boop", "both"),
    "should be the output of `fit\\(\\)`."
  )
  
  expect_error(
    get_p_value(null_fits, obs_fit$estimate, "both"),
    "should be the output of `fit\\(\\)`."
  )
  
  expect_error(
    get_p_value(obs_fit, null_fits, "both"),
    "be passed to `generate\\(\\)`"
  )
})

test_that("get_p_value errors informatively when args are switched", {
  # switch obs_stat and x
  obs_stat <- gss %>%
    specify(response = hours) %>%
    calculate(stat = "mean")
  
  set.seed(1)
  
  null_dist <- gss %>%
    specify(response = hours) %>%
    hypothesize(null = "point", mu = 41) %>%
    generate(reps = 20, type = "bootstrap") %>%
    calculate(stat = "mean")
  
  expect_error(
    get_p_value(obs_stat, null_dist, "both"),
    "mistakenly switched the order of `obs_stat` and `x`"
  )
  
  expect_silent(
    get_p_value(null_dist, obs_stat, "both")
  )
})

test_that("get_p_value can handle theoretical distributions", {
  # f ------------------------------------------------------------
  # direction = "right" is the only valid one
  f_dist <- 
    gss %>% 
    specify(age ~ partyid) %>%
    assume(
      distribution = "F", 
      c(length(unique(gss$partyid)) - 1, nrow(gss) - 1)
    )
  
  f_obs <- 
    gss %>% 
    specify(age ~ partyid) %>%
    calculate(stat = "F")
  
  expect_equal(
    get_p_value(f_dist, f_obs, direction = "right"),
    0.06005251,
    tolerance = 1e-5
  )

  old_way_f <- broom::tidy(aov(age ~ partyid, gss))
  
  expect_equal(
    get_p_value(f_dist, f_obs, direction = "right"),
    old_way_f$p.value[[1]],
    tolerance = 1e-5
  )
  
  # t ------------------------------------------------------------
  t_dist <- 
    gss %>%
    specify(response = hours) %>% 
    hypothesize(null = "point", mu = 40) %>%
    assume("t", nrow(gss) - 1)
  
  t_obs <-
    gss %>%
    specify(response = hours) %>% 
    hypothesize(null = "point", mu = 40) %>%
    calculate(stat = "t")
  
  expect_equal(
    get_p_value(t_dist, t_obs, direction = "both"),
    0.038,
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(t_dist, t_obs, direction = "left"),
    0.981,
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(t_dist, t_obs, direction = "right"),
    1 - get_p_value(t_dist, t_obs, direction = "left"),
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(t_dist, t_obs, direction = "both"),
    (1 - get_p_value(t_dist, t_obs, direction = "left")) * 2,
    tolerance = .001
  )

  old_way_both <- t_test(gss, hours ~ NULL, mu = 40, alternative = "two.sided")
  
  expect_equal(
    old_way_both$p_value, 
    get_p_value(t_dist, t_obs, direction = "both"),
    tolerance = 1e-5
  )
  
  old_way_left <- t_test(gss, hours ~ NULL, mu = 40, alternative = "less")
  
  expect_equal(
    old_way_left$p_value, 
    get_p_value(t_dist, t_obs, direction = "left")
  )
  
  old_way_right <- t_test(gss, hours ~ NULL, mu = 40, alternative = "greater")
  
  expect_equal(
    old_way_right$p_value, 
    get_p_value(t_dist, t_obs, direction = "right")
  )
  
  # chisq ------------------------------------------------------------
  # direction = "right" is the only valid one
  chisq_dist <- 
    gss %>% 
    specify(college ~ finrela) %>%
    assume(
      distribution = "Chisq", 
      df = (length(unique(gss$finrela)) - 1) * 
           (length(unique(gss$college)) - 1)
    )
  
  chisq_obs <- 
    gss %>% 
    specify(college ~ finrela) %>%
    calculate(stat = "Chisq")
  
  expect_equal(
    get_p_value(chisq_dist, chisq_obs, direction = "right"),
    1.082094e-05,
    tolerance = 1e-7
  )

  expect_warning(
    old_way <- chisq_test(gss, college ~ finrela)
  )
  
  expect_equal(
    old_way$p_value, 
    get_p_value(chisq_dist, chisq_obs, direction = "right"),
    tolerance = 1e-7
  )
  
  # z ------------------------------------------------------------
  z_dist <- 
    gss %>%
    specify(response = sex, success = "female") %>%
    hypothesize(null = "point", p = .5) %>%
    assume("z")
  
  z_obs <- 
    gss %>%
    specify(response = sex, success = "female") %>%
    hypothesize(null = "point", p = .5) %>%
    calculate(stat = "z")
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "both"),
    0.244,
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "left"),
    0.122,
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "right"),
    1 - get_p_value(z_dist, z_obs, direction = "left"),
    tolerance = .001
  )
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "both"),
    (1 - get_p_value(z_dist, z_obs, direction = "right")) * 2,
    tolerance = .001
  )
  
  old_way_z_both <- prop_test(gss, sex ~ NULL, success = "female", p = .5, 
                              alternative = "two.sided", z = TRUE)
  old_way_z_left <- prop_test(gss, sex ~ NULL, success = "female", p = .5,
                              alternative = "less", z = TRUE)
  old_way_z_right <- prop_test(gss, sex ~ NULL, success = "female", p = .5, 
                               alternative = "greater", z = TRUE)
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "both"),
    old_way_z_both$p_value
  )
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "left"),
    old_way_z_left$p_value
  )
  
  expect_equal(
    get_p_value(z_dist, z_obs, direction = "right"),
    old_way_z_right$p_value
  )
})
