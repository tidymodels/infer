context("pipe")

test_that("pipe works", {
  piped <- iris %>% specify(Sepal.Length ~ Sepal.Width)
  non_piped <- specify(iris, Sepal.Length ~ Sepal.Width)
  expect_equal(pipe, non_piped)
})