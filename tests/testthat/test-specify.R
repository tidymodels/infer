context("specify")

one_nonshift_mean <- mtcars_df %>% specify(response = mpg)

one_nonshift_prop <- mtcars_df %>% specify(response = am, success = "1")

two_means_boot <- mtcars_df %>% specify(mpg ~ am)

two_props_boot <- mtcars_df %>% specify(am ~ vs, success = "1")

slope_boot <- mtcars_df %>% specify(mpg ~ hp)

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
  expect_is(mtcars_df, "data.frame")
  expect_error(specify(mtcars_df, mtcars_df$mpg))
})

test_that("response and explanatory arguments", {
  expect_error(specify(mtcars_df, response = blah), "response.*cannot be found")
  expect_error(
    specify(mtcars_df, response = "blah"), "response.*bare.*not a string"
  )
  expect_error(
    specify(mtcars_df, formula = mpg ~ blah), "explanatory.*cannot be found"
  )
  expect_error(specify(mtcars_df, blah2 ~ cyl), "response.*cannot be found")
  expect_error(specify(mtcars_df), "Supply.*response")
  expect_error(specify(mtcars_df, formula = mpg ~ mpg), "different")
  expect_error(
    specify(mtcars_df, formula = "mpg" ~ cyl), "response.*bare.*not a string"
  )
  expect_error(
    specify(mtcars_df, formula = mpg ~ "cyl"), "explanatory.*bare.*not a string"
  )
  expect_silent(specify(mtcars_df, formula = mpg ~ cyl))

  expect_error(specify(mtcars_df, formula = NULL ~ cyl), "NULL.*response")
})

test_that("success argument", {
  expect_error(specify(mtcars_df, response = vs, success = 1))
  expect_error(specify(mtcars_df, response = vs, success = "bogus"))
  expect_error(specify(mtcars_df, response = mpg, success = "1"))
  expect_error(specify(mtcars_df, response = cyl, success = "4"))
  # success not given
  expect_error(specify(mtcars_df, response = am))

})

test_that("sensible output", {
  expect_equal(ncol(specify(mtcars_df, formula = mpg ~ NULL)), 1)
  expect_equal(ncol(specify(mtcars_df, formula = mpg ~ wt)), 2)
  expect_equal(class(specify(mtcars_df, formula = mpg ~ wt))[1], "infer")
})

test_that("formula argument is a formula", {
  expect_error(specify(mtcars_df, formula = "vs", success = 1))

  # Issue #110: https://github.com/tidymodels/infer/issues/110
  expect_error(specify(mtcars, am, success = "1"))
  expect_error(specify(mtcars, response = am, "1"))
  expect_silent({
    mtcars %>%
      dplyr::mutate(am = factor(am)) %>%
      specify(response = am, success = "1")
  })
})

test_that("is_complete works", {
  some_missing <- data.frame(vec = c(NA, 2, 3))
  expect_warning(specify(some_missing, response = vec))
})

test_that("specify doesn't have NSE issues (#256)", {
  expect_silent(specify(tibble(x = 1:10), x ~ NULL))
})
