context("specify")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
                vs = factor(vs),
                am = factor(am),
                gear = factor(gear),
                carb = factor(carb))

one_nonshift_mean <- mtcars %>%
  specify(response = mpg)

one_nonshift_prop <- mtcars %>%
  specify(response = am, success = "1")

two_means_boot <- mtcars %>%
  specify(mpg ~ am)

two_props_boot <- mtcars %>%
  specify(am ~ vs, success = "1")

slope_boot <- mtcars %>%
  specify(mpg ~ hp)

test_that("auto `type` works (specify)", {
  expect_equal(attr(one_nonshift_mean, "type"), "bootstrap")
  expect_equal(attr(one_nonshift_prop, "type"), "bootstrap")
  expect_equal(attr(two_means_boot, "type"), "bootstrap")
  expect_equal(attr(two_props_boot, "type"), "bootstrap")
  expect_equal(attr(slope_boot, "type"), "bootstrap")
})

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
  expect_error(specify(mtcars, formula = mpg ~ "cyl"))
  expect_silent(specify(mtcars, formula = mpg ~ cyl))

})

test_that("success argument", {

  expect_error(specify(mtcars, response = vs, success = 1))
  expect_error(specify(mtcars, response = vs, success = "bogus"))
  expect_error(specify(mtcars, response = mpg, success = "1"))
  expect_error(specify(mtcars, response = cyl, success = "4"))
  # success not given
  expect_error(specify(mtcars, response = am))

})

test_that("sensible output", {
  expect_equal(ncol(specify(mtcars, formula = mpg ~ NULL)), 1)
  expect_equal(ncol(specify(mtcars, formula = mpg ~ wt)), 2)
  expect_equal(class(specify(mtcars, formula = mpg ~ wt))[1], "infer")
})

test_that("formula argument is a formula", {
  expect_error(specify(mtcars, formula = "vs", success = 1))
})

test_that("is_complete works", {
  some_missing <- data.frame(vec = c(NA, 2, 3))
  expect_warning(specify(some_missing, response = vec))
})
