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
  expect_snapshot(error = TRUE, specify(blah ~ cyl))
  expect_snapshot(error = TRUE, specify(1:3))
  expect_s3_class(mtcars_df, "data.frame")
  expect_snapshot(error = TRUE, specify(mtcars_df, mtcars_df$mpg))
})

test_that("response and explanatory arguments", {
  expect_snapshot(error = TRUE, specify(mtcars_df, response = blah))
  expect_snapshot(error = TRUE, specify(mtcars_df, response = "blah"))
  expect_snapshot(error = TRUE, specify(mtcars_df, formula = mpg ~ blah))
  expect_snapshot(error = TRUE, specify(mtcars_df, blah2 ~ cyl))
  expect_snapshot(error = TRUE, specify(mtcars_df))
  expect_snapshot(error = TRUE, specify(mtcars_df, formula = mpg ~ mpg))
  expect_snapshot(error = TRUE, specify(mtcars_df, formula = "mpg" ~ cyl))
  expect_snapshot(error = TRUE, specify(mtcars_df, formula = mpg ~ "cyl"))
  expect_silent(specify(mtcars_df, formula = mpg ~ cyl))

  expect_snapshot(error = TRUE, specify(mtcars_df, formula = NULL ~ cyl))
})

test_that("success argument", {
  expect_snapshot(error = TRUE, specify(mtcars_df, response = vs, success = 1))
  expect_snapshot(
    error = TRUE,
    specify(mtcars_df, response = vs, success = "bogus")
  )
  expect_snapshot(
    error = TRUE,
    specify(mtcars_df, response = mpg, success = "1")
  )
  expect_snapshot(
    error = TRUE,
    specify(mtcars_df, response = cyl, success = "4")
  )
  # success not given
  expect_snapshot(error = TRUE, specify(mtcars_df, response = am))
})

test_that("sensible output", {
  expect_equal(ncol(specify(mtcars_df, formula = mpg ~ NULL)), 1)
  expect_equal(ncol(specify(mtcars_df, formula = mpg ~ wt)), 2)
  expect_equal(class(specify(mtcars_df, formula = mpg ~ wt))[1], "infer")
})

test_that("formula argument is a formula", {
  expect_snapshot(error = TRUE, specify(mtcars_df, formula = "vs", success = 1))

  # Issue #110: https://github.com/tidymodels/infer/issues/110
  expect_snapshot(error = TRUE, specify(mtcars, am, success = "1"))
  expect_snapshot(error = TRUE, specify(mtcars, response = am, "1"))
  expect_silent({
    mtcars %>%
      dplyr::mutate(am = factor(am)) %>%
      specify(response = am, success = "1")
  })
})

test_that("is_complete works", {
  some_missing <- data.frame(vec = c(NA, 2, 3))
  expect_snapshot(res_ <- specify(some_missing, response = vec))
})

test_that("specify doesn't have NSE issues (#256)", {
  expect_silent(specify(tibble(x = 1:10), x ~ NULL))
})

test_that("specify messages when dropping unused levels", {
  expect_snapshot(
    res_ <- gss %>%
      dplyr::filter(partyid %in% c("rep", "dem")) %>%
      specify(age ~ partyid)
  )

  expect_snapshot(
    res_ <- gss %>%
      dplyr::filter(partyid %in% c("rep", "dem")) %>%
      specify(partyid ~ age)
  )

  expect_snapshot(
    res_ <- gss %>%
      dplyr::filter(partyid %in% c("rep", "dem")) %>%
      specify(partyid ~ NULL)
  )

  expect_silent(
    gss %>%
      dplyr::filter(partyid %in% c("rep", "dem")) %>%
      specify(age ~ NULL)
  )
})

test_that("user can specify multiple explanatory variables", {
  x <- gss %>% specify(hours ~ sex + college)

  expect_true(inherits(x, "infer"))
  expect_true(inherits(explanatory_variable(x), "tbl_df"))
  expect_true(inherits(explanatory_name(x), "character"))
  expect_true(inherits(explanatory_expr(x), "call"))

  expect_equal(explanatory_name(x), c("sex", "college"))
  expect_equal(response_name(x), "hours")
})
