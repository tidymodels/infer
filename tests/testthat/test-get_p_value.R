context("get_p_value")

set.seed(2018)
test_df <- tibble::tibble(
  stat = sample(c(
    -5, -4, -4, -4, -1, -0.5, rep(0, 6), 1, 1, 3.999, 4, 4, 4.001, 5, 5
  ))
)

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
  stat_df <- tibble::tibble(stat = 1:10)
  
  expect_warning(
    get_p_value(stat_df, obs_stat = -10, direction = "left"),
    "be cautious"
  )
})

test_that("get_p_value throws error in case of `NaN` stat", {
  stat_df <- tibble::tibble(stat = 1:10)
  obs_stat <- 2.71
  
  stat_df$stat[1] <- NaN
  expect_error(
    get_p_value(stat_df, obs_stat, "both"),
    "1 calculated statistic was `NaN`.*not well-defined"
  )
  
  stat_df$stat[2] <- NaN
  expect_error(
    get_p_value(stat_df, obs_stat, "both"),
    "2 calculated statistics were `NaN`.*not well-defined"
  )
  
  # In the case that _all_ values are NaN, error should have different text
  stat_df$stat <- NaN
  expect_error(get_p_value(stat_df, obs_stat, "both"), "All calculated stat")
})
