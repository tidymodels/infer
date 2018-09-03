context("conf_int")

test_that("basics work", {
  expect_silent(test_df %>% get_confidence_interval())
  expect_error(test_df %>% get_confidence_interval(type = "other"))
  expect_error(test_df %>% get_confidence_interval(level = 1.2))
  expect_error(test_df %>% get_confidence_interval(point_estimate = "help"))

  expect_silent(iris_calc %>% 
                  get_confidence_interval(type = "se", 
                                          point_estimate = 4))
  expect_silent(iris_calc %>% 
                  get_confidence_interval(type = "se", 
                                          point_estimate = obs_diff))
  expect_error(iris_calc %>% 
                 get_confidence_interval(type = "se", 
                                         point_estimate = "error"))
  expect_error(iris_calc %>% 
                 get_confidence_interval(type = "se"))
})
