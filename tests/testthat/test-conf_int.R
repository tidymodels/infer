context("conf_int")

obs_diff <- iris_tbl %>%
  specify(Sepal.Length.Group ~ Sepal.Width.Group, success = "<=5") %>%
  calculate(stat = "diff in props", order = c("large", "small"))

set.seed(2018)
test_df <- tibble::tibble(stat = rnorm(100))

test_that("basics work", {
  expect_silent(test_df %>% conf_int())
  expect_error(test_df  %>% conf_int(type = "other"))
  expect_error(test_df  %>% conf_int(level = 1.2))
  expect_error(test_df  %>% conf_int(point_estimate = "help"))
  
  expect_silent(iris_calc %>% get_ci(type = "se", point_estimate = 4))
  expect_silent(iris_calc %>% get_ci(type = "se", point_estimate = obs_diff))
  expect_error(iris_calc  %>% get_ci(type = "se", point_estimate = "error"))
  expect_error(iris_calc  %>% get_ci(type = "se"))
})
