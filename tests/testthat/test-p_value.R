context("p_value")

iris_tbl <- iris %>%
  dplyr::mutate(
    Sepal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5"),
    Sepal.Width.Group = dplyr::if_else(Sepal.Width > 3, "large", "small")
  )

iris_calc <- iris_tbl %>%
  specify(Sepal.Length.Group ~ Sepal.Width.Group, success = "<=5") %>%
  hypothesize(null = "independence") %>%
  generate(reps = 1000) %>%
  calculate(stat = "diff in props", order = c("large", "small"))

set.seed(2018)
test_df <- tibble::tibble(stat = rnorm(100))

test_that("direction is appropriate", {
  expect_error(test_df %>% p_value(obs_stat = 0.5, direction = "righ"))
})

test_that("p_value makes sense", {
  expect_lt(
    iris_calc %>%
      p_value(obs_stat = 0.1, direction = "right") %>%
      dplyr::pull(),
    expected = 0.1
  )
  expect_gt(
    iris_calc %>%
      p_value(obs_stat = -0.1, direction = "greater") %>%
      dplyr::pull(),
    expected = 0.9
  )
  expect_equal(
    iris_calc %>%
      p_value(obs_stat = median(iris_calc$stat), direction = "both") %>%
      dplyr::pull(),
    expected = 1
  )
  expect_lt(
    iris_calc %>%
      p_value(obs_stat = -0.2, direction = "left") %>%
      dplyr::pull(),
    expected = 0.02
  )
  expect_gt(
    iris_calc %>%
      p_value(obs_stat = -0.2, direction = "right") %>%
      dplyr::pull(),
    expected = 0.98
  )
  expect_equal(
    iris_calc %>%
      get_pvalue(obs_stat = median(iris_calc$stat) + 1, 
        direction = "two_sided") %>%
      dplyr::pull(),
    expected = 0
  )
})
