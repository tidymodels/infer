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
    "`x` should be the result of calling `generate\\(\\)`."
  )
})
