context("specify")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
                vs = factor(vs),
                am = factor(am),
                gear = factor(gear),
                carb = factor(carb))

test_that("data argument", {

  expect_error(specify(blah ~ cyl))
  expect_error(specify(1:3))

})

test_that("response and explanatory arguments", {

  expect_error(specify(mtcars, response = blah))
  expect_error(specify(mtcars, response = "blah"))
  expect_error(specify(mtcars, formula = mpg ~ blah))
  expect_error(specify(blah ~ cyl))
  expect_error(specify(mtcars_f, blah2 ~ cyl))
  expect_error(specify(mtcars))
  expect_error(specify(mtcars, formula = mpg ~ mpg))

})

test_that("success argument", {

  expect_error(specify(mtcars, response = vs, success = 1))
  expect_error(specify(mtcars, response = vs, success = "bogus"))

})

test_that("sensible output", {
  expect_equal(ncol(specify(mtcars, formula = mpg ~ NULL)), 1)
  expect_equal(ncol(specify(mtcars, formula = mpg ~ wt)), 2)
  expect_equal(class(specify(mtcars, formula = mpg ~ wt))[1], "infer")
})

test_that("formula argument is a formula", {
  expect_error(specify(mtcars, formula = "vs", success = 1))
})