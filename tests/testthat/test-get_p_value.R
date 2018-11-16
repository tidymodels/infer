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
  expect_equal(get_p_value(test_df, 4, "right")[[1]][1], 5/20)
  expect_equal(get_p_value(test_df, 4, "left")[[1]][1], 17/20)
  expect_equal(get_p_value(test_df, 4, "both")[[1]][1], 10/20)
  
  expect_equal(get_p_value(test_df, 0, "right")[[1]][1], 14/20)
  expect_equal(get_p_value(test_df, 0, "left")[[1]][1], 12/20)
  # This is also a check for not returning value more than 1
  expect_equal(get_p_value(test_df, 0, "both")[[1]][1], 1)
  
  expect_equal(get_p_value(test_df, -3.999, "right")[[1]][1], 16/20)
  expect_equal(get_p_value(test_df, -3.999, "left")[[1]][1], 4/20)
  expect_equal(get_p_value(test_df, -3.999, "both")[[1]][1], 8/20)
  
  expect_equal(
    get_p_value(test_df, 4, "greater"), get_p_value(test_df, 4, "right")
  )
  expect_equal(get_p_value(test_df, 4, "less"), get_p_value(test_df, 4, "left"))
  expect_equal(
    get_p_value(test_df, 4, "two_sided"), get_p_value(test_df, 4, "both")
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
