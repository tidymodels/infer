context("visualize")

Sepal.Width_resamp <- iris %>%
  specify(Sepal.Width ~ NULL) %>%
  hypothesize(null = "point", med = 3) %>%
  generate(reps = 10, type = "bootstrap") %>% 
  calculate(stat = "median") 

test_that("visualize print histogram", {
  expect_silent(visualize(Sepal.Width_resamp))
  expect_error(
    Sepal.Width_resamp %>% visualize(bins = "yep")
  )
})
