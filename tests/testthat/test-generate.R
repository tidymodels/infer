context("generate")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

test_that("generate arguments", {
  
  expect_silent(generate(mtcars, reps = 1, type = "bootstrap"))
  
})