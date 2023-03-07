one_mean <- mtcars_df %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", mu = 25)

one_mean_specify <- mtcars_df %>%
  specify(response = mpg)

one_median <- mtcars_df %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", med = 26)

one_prop <- mtcars_df %>%
  specify(response = am, success = "1") %>% # formula alt: am ~ NULL
  hypothesize(null = "point", p = .25)

one_prop_specify <- mtcars_df %>%
  specify(response = am, success = "1")

two_props <- mtcars_df %>%
  specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
  hypothesize(null = "independence")

gof_chisq <- mtcars_df %>%
  specify(cyl ~ NULL) %>% # alt: response = cyl
  hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25))

indep_chisq <- mtcars_df %>%
  specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
  hypothesize(null = "independence")

two_means <- mtcars_df %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence")

two_medians <- mtcars_df %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence")

anova_f <- mtcars_df %>%
  specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence")

slopes <- mtcars_df %>%
  specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence")

test_that("auto `type` works (hypothesize)", {
  expect_equal(attr(one_mean, "type"), "bootstrap")
  expect_equal(attr(one_median, "type"), "bootstrap")
  expect_equal(attr(one_prop, "type"), "draw")
  expect_equal(attr(two_props, "type"), "permute")
  expect_equal(attr(gof_chisq, "type"), "draw")
  expect_equal(attr(indep_chisq, "type"), "permute")
  expect_equal(attr(two_means, "type"), "permute")
  expect_equal(attr(two_medians, "type"), "permute")
  expect_equal(attr(anova_f, "type"), "permute")
  expect_equal(attr(slopes, "type"), "permute")
})

test_that(
  "hypothesize() throws an error when null is not point or independence", {
  expect_error(
    mtcars_df %>%
      specify(response = mpg) %>%
      hypothesize(null = "dependence"),
    '`null` should be either "point" or "independence".'
  )
})

test_that(
  "hypothesize() allows partial matching of null arg for point", {
  hyp_p <- mtcars_df %>%
    specify(response = mpg) %>%
    hypothesize(null = "p", mu = 0)
  expect_equal(attr(hyp_p, "null"), "point")
})

test_that(
  "hypothesize() allows partial matching of null arg for independence", {
  hyp_i <- mtcars_df %>%
    specify(mpg ~ vs) %>%
    hypothesize(null = "i")
  expect_equal(attr(hyp_i, "null"), "independence")
})

test_that(
  "hypothesize() throws an error when multiple null values are provided", {
  expect_error(
    mtcars_df %>%
      specify(response = mpg) %>%
      hypothesize(null = c("point", "independence")),
    "You should specify exactly one type of null hypothesis"
  )
})

test_that(
  "hypothesize() throws an error when multiple params are set", {
  expect_error(
    mtcars_df %>%
      specify(response = mpg) %>%
      hypothesize(null = "point", mu = 25, med = 20),
    "You must specify exactly one of `p`, `mu`, `med`, or `sigma`"
  )
})

test_that(
  "hypothesize() throws a warning when params are set with independence", {
  expect_warning(
    mtcars_df %>%
      specify(mpg ~ vs) %>%
      hypothesize(null = "independence", mu = 25),
    'Parameter values are not specified when testing that two variables are independent.'
  )
})

test_that(
  "hypothesize() throws an error when p is greater than 1", {
  expect_error(
    mtcars_df %>%
      specify(response = vs, success = "1") %>%
      hypothesize(null = "point", p = 1 + .Machine$double.eps),
    "`p` should only contain values between zero and one."
  )
})

test_that(
  "hypothesize() throws an error when p is less than 0", {
  expect_error(
    mtcars_df %>%
      specify(response = vs, success = "1") %>%
      hypothesize(null = "point", p = - .Machine$double.neg.eps),
    "`p` should only contain values between zero and one."
  )
})

test_that(
  "hypothesize() throws an error when p contains missing values", {
  expect_error(
    mtcars_df %>%
      specify(response = vs, success = "1") %>%
      hypothesize(null = "point", p = c("0" = 0.5, "1" = NA_real_)),
    "`p` should not contain missing values"
  )
})

test_that(
  "hypothesize() throws an error when vector p does not sum to 1", {
  expect_error(
    mtcars_df %>%
      specify(response = vs, success = "1") %>%
      hypothesize(null = "point", p = c("0" = 0.5, "1" = 0.5 + (eps *2))),
    "Make sure the hypothesized values for the `p` parameters sum to 1. Please try again."
  )
})

test_that("hypothesize arguments function", {
  mtcars_f <- dplyr::mutate(mtcars, cyl = factor(cyl))
  mtcars_s <- mtcars_f %>% specify(response = mpg)
  matrix1 <- matrix(data = NA, nrow = 3, ncol = 3)

  expect_error(hypothesize(matrix1))
  expect_error(hypothesize(mtcars_s, null = NA))
  expect_error(hypothesize(mtcars_s))

  expect_error(mtcars_s %>% hypothesize(null = "point", mean = 3))

  expect_error(mtcars_s %>% hypothesize(null = "independence"))
  expect_error(mtcars_s %>% hypothesize(null = "point"))
  # Produces error on win-build
#   expect_warning(
#     mtcars_s %>% hypothesize(null = c("point", "independence"), mu = 3)
#   )

  expect_error(
    mtcars_df %>% dplyr::select(vs) %>% hypothesize(null = "point", mu = 1)
  )

  expect_error(
    mtcars_df %>% specify(response = vs) %>% hypothesize(null = "point", mu = 1)
  )

  expect_error(mtcars_s %>% hypothesize(null = "point", p = 0.2))

  expect_error(mtcars_s %>% hypothesize())
})

test_that("params correct", {
  expect_error(hypothesize(one_prop_specify, null = "point", mu = 2))
  expect_error(hypothesize(one_mean_specify, null = "point", mean = 0.5))
})

test_that("sensible output", {
  expect_equal(class(one_mean)[1], "infer")
})

test_that("user can specify multiple explanatory variables", {
  x <-
    gss %>%
    specify(hours ~ sex + college) %>%
    hypothesize(null = "independence")

  expect_true(inherits(x, "infer"))
  expect_true(inherits(explanatory_variable(x), "tbl_df"))
  expect_true(inherits(explanatory_name(x), "character"))
  expect_true(inherits(explanatory_expr(x), "call"))

  expect_equal(explanatory_name(x), c("sex", "college"))
  expect_equal(response_name(x), "hours")

  expect_warning(
    gss %>%
      specify(hours ~ sex + college) %>%
      hypothesize(null = "independence", mu = 40),
    "Parameter values are not specified when testing"
  )
})

# is_hypothesized ---------------------------------------------------------
test_that("is_hypothesized works", {
  expect_true(is_hypothesized(one_mean))
  expect_false(is_hypothesized(one_mean_specify))
})

