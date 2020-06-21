context("conf_int")

test_that("get_confidence_interval works", {
  perc_basic_out <- tibble::tibble(
    `2.5%`  = quantile(test_df[["stat"]], 0.025),
    `97.5%` = quantile(test_df[["stat"]], 0.975)
  )
  expect_equal(test_df %>% get_confidence_interval(), perc_basic_out)
  
  # Type "percentile"
  expect_equal(
    test_df %>% get_confidence_interval(type = "percentile"), perc_basic_out
  )
  expect_equal(
    test_df %>% get_confidence_interval(level = 0.5, type = "percentile"),
    tibble::tibble(
      `25%` = quantile(test_df[["stat"]], 0.25),
      `75%` = quantile(test_df[["stat"]], 0.75)
    )
  )
  
  # Type "se"
  point <- mean(test_df[["stat"]])
  expect_equal(
    test_df %>% get_confidence_interval(type = "se", point_estimate = point),
    tibble::tibble(lower = -1.965, upper = 2.008),
    tolerance = 1e-3
  )
  expect_equal(
    test_df %>%
      get_confidence_interval(level = 0.5, type = "se", point_estimate = point),
    tibble::tibble(lower = -0.662, upper = 0.705),
    tolerance = 1e-3
  )
})

test_that("get_confidence_interval checks input", {
  expect_error(test_df %>% get_confidence_interval(type = "other"), "`type`")
  expect_error(test_df %>% get_confidence_interval(level = 1.2), "`level`")
  
  expect_error(
    test_df %>% get_confidence_interval(point_estimate = "a"),
    "`point_estimate`"
  )
  expect_error(
    gss_calc %>% get_confidence_interval(type = "se", point_estimate = "a"),
    "`point_estimate`"
  )
  
  expect_error(
    gss_calc %>% get_confidence_interval(type = "se"),
    '`point_estimate`.*type = "se"'
  )
})
