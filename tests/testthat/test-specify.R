context("specify")

test_that("specify arguments", {
  
  expect_error(specify(mtcars, response = blah))
  expect_equal(ncol(specify(mtcars, formula = mpg ~ wt)), 2)
  expect_error(specify(mtcars, formula = mpg ~ blah))
  expect_equal(class(specify(mtcars, formula = mpg ~ wt))[1], "infer")
  expect_error(specify(blah ~ cyl))
  expect_error(specify(mtcars_f, blah2 ~ cyl))
  
  
})