context("hypothesize")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

test_that("hypothesize arguments",{
  
  mtcars_f <- dplyr::mutate(mtcars, cyl = factor(cyl))
  mtcars_s <- mtcars_f %>% specify(response = mpg)
  blah <- matrix(data = NA, nrow = 3, ncol = 3)
  
  expect_error(hypothesize(mtcars_s, null = NA))
  expect_warning(hypothesize(mtcars_s))
  
})