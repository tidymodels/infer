context("get_confidence_interval")

set.seed(2018)
test_df <- gss_calc[1:20,]
test_df$stat <- c(
  -5, -4, -4, -4, -1, -0.5, rep(0, 6), 1, 1, 3.999, 4, 4, 4.001, 5, 5
)
point <- mean(test_df[["stat"]])

perc_def_out <- tibble::tibble(
  lower_ci = unname(quantile(test_df[["stat"]], 0.025)),
  upper_ci = unname(quantile(test_df[["stat"]], 0.975))
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
      lower_ci = unname(quantile(test_df[["stat"]], 0.25)),
      upper_ci = unname(quantile(test_df[["stat"]], 0.75))
    )
  )
})
  
test_that("get_confidence_interval works with `type = 'se'`", {  
  expect_message(
    expect_equal(
      test_df %>% get_confidence_interval(type = "se", point_estimate = point),
      tibble::tibble(lower_ci = -5.653, upper_ci = 6.603),
      tolerance = 1e-3
    ),
    "Using `level = 0.95`"
  )
  
  expect_equal(
    test_df %>%
      get_confidence_interval(level = 0.5, type = "se", point_estimate = point),
    tibble::tibble(lower_ci = -1.633, upper_ci = 2.583),
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
      tibble::tibble(lower_ci = -4.00, upper_ci = 5),
      tolerance = 1e-3
    ),
    "Using `level = 0.95`"
  )
  
  expect_equal(
    test_df %>%
      get_confidence_interval(
        level = 0.5, type = "bias-corrected", point_estimate = point
      ),
    tibble::tibble(lower_ci = 0, upper_ci = 4.0007),
    tolerance = 1e-3
  )
})

test_that("get_confidence_interval supports data frame `point_estimate`", {
  point_df <- data.frame(p = point)
  
  expect_equal(
    test_df %>% get_confidence_interval(type = "se", point_estimate = point),
    test_df %>% get_confidence_interval(type = "se", point_estimate = point_df),
    tolerance = eps
  )
  expect_equal(
    test_df %>%
      get_confidence_interval(type = "bias-corrected", point_estimate = point),
    test_df %>%
      get_confidence_interval(
        type = "bias-corrected", point_estimate = point_df
      ),
    tolerance = eps
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
