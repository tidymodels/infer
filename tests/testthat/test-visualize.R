context("visualize")

sepal_width_resamp <- iris %>%
  specify(Sepal.Width ~ NULL) %>%
  hypothesize(null = "point", med = 3) %>%
  generate(reps = 10, type = "bootstrap") %>% 
  calculate(stat = "median") 

test_that("visualize print histogram", {
  expect_silent(visualize(sepal_width_resamp))
})