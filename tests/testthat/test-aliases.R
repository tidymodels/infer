context("aliases")

test_that("aliases work", {
  expect_gt(
    iris_calc %>%
      get_pvalue(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 0.98
  )
  
  expect_silent(test_df %>% get_ci())
})

test_that("old aliases produce warning", {
  expect_warning(
    iris_calc %>%
      p_value(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 0.98
  )
  
  expect_warning(test_df %>% conf_int())
  
})