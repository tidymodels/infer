context("hypothesize")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

one_mean <- mtcars %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", mu = 25)

one_mean_specify <- mtcars %>%
  specify(response = mpg)

one_median <- mtcars %>%
  specify(response = mpg) %>% # formula alt: mpg ~ NULL
  hypothesize(null = "point", med = 26)

one_prop <- mtcars %>%
  specify(response = am, success = "1") %>% # formula alt: am ~ NULL
  hypothesize(null = "point", p = .25)

one_prop_specify <- mtcars %>%
  specify(response = am, success = "1")

two_props <- mtcars %>%
  specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
  hypothesize(null = "independence")

gof_chisq <- mtcars %>%
  specify(cyl ~ NULL) %>% # alt: response = cyl
  hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25))

indep_chisq <- mtcars %>%
  specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
  hypothesize(null = "independence")

two_means <- mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence")

two_medians <- mtcars %>%
  specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
  hypothesize(null = "independence")

anova_f <- mtcars %>%
  specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence")

slopes <- mtcars %>%
  specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
  hypothesize(null = "independence")

test_that("auto `type` works (hypothesize)", {
  expect_equal(attr(one_mean, "type"), "bootstrap")
  expect_equal(attr(one_median, "type"), "bootstrap")
  expect_equal(attr(one_prop, "type"), "simulate")
  expect_equal(attr(two_props, "type"), "permute")
  expect_equal(attr(gof_chisq, "type"), "simulate")
  expect_equal(attr(indep_chisq, "type"), "permute")
  expect_equal(attr(two_means, "type"), "permute")
  expect_equal(attr(two_medians, "type"), "permute")
  expect_equal(attr(anova_f, "type"), "permute")
  expect_equal(attr(slopes, "type"), "permute")
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
#  expect_warning(mtcars_s %>%
#                   hypothesize(null = c("point", "independence"), mu = 3))

  expect_error(mtcars %>% dplyr::select(vs) %>%
                 hypothesize(null = "point", mu = 1))

  expect_error(mtcars %>% specify(response = vs) %>%
                 hypothesize(null = "point", mu = 1))

  expect_error(mtcars %>% specify(response = vs, success = "1") %>%
                 hypothesize(null = "point", p = 1.1))
  expect_error(mtcars %>% specify(response = vs, success = "1") %>%
                 hypothesize(null = "point", p = -23))

  expect_error(mtcars_s %>%
                 hypothesize(null = "point",
                             p = c("4" = .2, "6" = .25, "8" = .25)))

  expect_error(mtcars_s %>% hypothesize(null = "point", p = 0.2))
  expect_warning(mtcars %>% specify(mpg ~ vs) %>%
                   hypothesize(null = "independence", p = 0.5))

  expect_error(mtcars_s %>% hypothesize())
})

test_that("params correct", {
  expect_error(hypothesize(one_prop_specify,
                           null = "point", mu = 2))
  expect_error(hypothesize(one_mean_specify,
                           null = "point", mean = 0.5))
})
