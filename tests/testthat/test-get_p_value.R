context("get_p_value")

set.seed(2018)
test_df <- tibble::tibble(stat = rnorm(100))

test_that("direction is appropriate", {
  expect_error(test_df %>% get_p_value(obs_stat = 0.5, direction = "righ"))
})

test_that("get_p_value makes sense", {
  expect_silent(
    test_df %>% 
      get_p_value(obs_stat = 0.7, direction = "right")
    )
  expect_lt(
    iris_calc %>%
      get_p_value(obs_stat = 0.1, direction = "right") %>%
      dplyr::pull(),
    expected = 0.1
  )
  expect_gt(
    iris_calc %>%
      get_p_value(obs_stat = -0.1, direction = "greater") %>%
      dplyr::pull(),
    expected = 0.9
  )
  expect_equal(
    iris_calc %>%
      get_p_value(obs_stat = median(iris_calc$stat), direction = "both") %>%
      dplyr::pull(),
    expected = 1
  )
  expect_lt(
    iris_calc %>%
      get_p_value(obs_stat = -0.2, direction = "left") %>%
      dplyr::pull(),
    expected = 0.02
  )
  expect_gt(
    iris_calc %>%
      get_p_value(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 0.98
  )
  expect_equal(
    iris_calc %>%
      get_p_value(
        obs_stat = median(iris_calc$stat) + 1, direction = "two_sided"
      ) %>%
      dplyr::pull(),
    expected = 0
  )
  expect_error(
    iris_calc %>%
      get_p_value(
        obs_stat = median(iris_calc$stat) + 1, direction = "wrong"
      )
  )
})

test_that("theoretical p-value not supported error", {
  obs_F <- iris_tbl %>% 
    specify(Sepal.Width ~ Species) %>%
    calculate(stat = "F") 
  expect_error(
    iris_tbl %>% 
      specify(Sepal.Width ~ Species) %>%
      hypothesize(null = "independence") %>% 
      calculate(stat = "F") %>% 
      get_p_value(obs_stat = obs_F, direction = "right")
  )
  
})