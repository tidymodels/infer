context("get_confidence_interval")

point <- mean(test_df[["stat"]])

perc_def_out <- tibble::tibble(
  `2.5%`  = unname(quantile(test_df[["stat"]], 0.025)),
  `97.5%` = unname(quantile(test_df[["stat"]], 0.975))
)

test_that("get_confidence_interval works with defaults", {
  expect_message(
    expect_equal(test_df %>% get_confidence_interval(), perc_def_out),
    "Using `level = 0.95`"
  )
})

test_that("get_confidence_interval works with `type = 'percentile'`", {
  expect_message(
    expect_equal(
      test_df %>% get_confidence_interval(type = "percentile"), perc_def_out
    ),
    "Using `level = 0.95`"
  )
  
  expect_equal(
    test_df %>% get_confidence_interval(level = 0.5, type = "percentile"),
    tibble::tibble(
      `25%` = unname(quantile(test_df[["stat"]], 0.25)),
      `75%` = unname(quantile(test_df[["stat"]], 0.75))
    )
  )
})
  
test_that("get_confidence_interval works with `type = 'se'`", {  
  expect_message(
    expect_equal(
      test_df %>% get_confidence_interval(type = "se", point_estimate = point),
      tibble::tibble(lower = -1.965, upper = 2.008),
      tolerance = 1e-3
    ),
    "Using `level = 0.95`"
  )
  
  expect_equal(
    test_df %>%
      get_confidence_interval(level = 0.5, type = "se", point_estimate = point),
    tibble::tibble(lower = -0.662, upper = 0.705),
    tolerance = 1e-3
  )
})

test_that("get_confidence_interval works with `type = 'bias-corrected'`", {
  expect_message(
    expect_equal(
      test_df %>%
        get_confidence_interval(
          type = "bias-corrected", point_estimate = point
        ),
      tibble::tibble(lower = -1.692, upper = 2.276),
      tolerance = 1e-3
    ),
    "Using `level = 0.95`"
  )
  
  expect_equal(
    test_df %>%
      get_confidence_interval(
        level = 0.5, type = "bias-corrected", point_estimate = point
      ),
    tibble::tibble(lower = -0.594, upper = 0.815),
    tolerance = 1e-3
  )
})

test_that("get_confidence_interval supports data frame `point_estimate`", {
  point_df <- data.frame(p = point)
  
  expect_equal(
    test_df %>% get_confidence_interval(type = "se", point_estimate = point),
    test_df %>% get_confidence_interval(type = "se", point_estimate = point_df)
  )
  expect_equal(
    test_df %>%
      get_confidence_interval(type = "bias-corrected", point_estimate = point),
    test_df %>%
      get_confidence_interval(
        type = "bias-corrected", point_estimate = point_df
      )
  )
})

test_that("get_confidence_interval messages with no explicit `level`", {
  expect_message(get_confidence_interval(test_df), "Using `level = 0.95`")
  expect_silent(get_confidence_interval(test_df, level = 0.95))
  expect_silent(get_confidence_interval(test_df, 0.95))
})

test_that("get_confidence_interval checks input", {
  expect_error(test_df %>% get_confidence_interval(type = "other"), "`type`")
  expect_error(test_df %>% get_confidence_interval(level = 1.2), "`level`")
  
  expect_error(
    test_df %>% get_confidence_interval(point_estimate = "a"),
    "`point_estimate`"
  )
  expect_error(
    test_df %>% get_confidence_interval(type = "se", point_estimate = "a"),
    "`point_estimate`"
  )
  expect_error(
    test_df %>%
      get_confidence_interval(
        type = "se", point_estimate = data.frame(p = "a")
      ),
    "`point_estimate\\[\\[1\\]\\]\\[\\[1\\]\\]`"
  )
  
  expect_error(
    test_df %>% get_confidence_interval(type = "se"), '`point_estimate`.*"se"'
  )
  expect_error(
    test_df %>% get_confidence_interval(type = "bias-corrected"),
    '`point_estimate`.*"bias-corrected"'
  )
})
