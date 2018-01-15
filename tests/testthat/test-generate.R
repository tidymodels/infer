context("generate")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

hyp_prop <- mtcars %>%
  specify(response = am, success = "1") %>%
  hypothesize(null = "point", p = .5)

hyp_diff_in_props <- mtcars %>%
  specify(am ~ vs, success = "1") %>%
  hypothesize(null = "independence")

hyp_chisq_gof <- mtcars %>%
  specify(response = cyl) %>%
  hypothesize(null = "point", p = c("4" = 1/3, "6" = 1/3, "8" = 1/3))

hyp_chisq_ind <- mtcars %>%
  specify(cyl ~ vs) %>%
  hypothesize(null = "independence")

hyp_mean <- mtcars %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", mu = 3)

hyp_median <- mtcars %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", med = 3)

hyp_sd <- mtcars %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", sigma = 7)

hyp_diff_in_means <- mtcars %>%
  specify(mpg ~ vs) %>%
  hypothesize(null = "independence")

hyp_anova <- mtcars %>%
  specify(mpg ~ cyl) %>%
  hypothesize(null = "independence")

test_that("cohesion with type argument", {

  expect_error(generate(hyp_prop, type = "bootstrap"))
  expect_error(generate(hyp_diff_in_props, type = "bootstrap"))
  expect_error(generate(hyp_chisq_gof, type = "bootstrap"))
  expect_error(generate(hyp_chisq_ind, type = "bootstrap"))
  expect_silent(generate(hyp_mean, type = "bootstrap"))
  expect_silent(generate(hyp_median, type = "bootstrap"))
  expect_silent(generate(hyp_sd, type = "bootstrap"))
  expect_error(generate(hyp_diff_in_means, type = "bootstrap"))
  expect_error(generate(hyp_anova, type = "bootstrap"))

  expect_silent(generate(hyp_prop, type = "simulate"))
  expect_error(generate(hyp_diff_in_props, type = "simulate"))
  expect_silent(generate(hyp_chisq_gof, type = "simulate"))
  expect_error(generate(hyp_chisq_ind, type = "simulate"))
  expect_error(generate(hyp_mean, type = "simulate"))
  expect_error(generate(hyp_diff_in_means, type = "simulate"))
  expect_error(generate(hyp_anova, type = "simulate"))

  expect_error(generate(hyp_prop, type = "permute"))
  expect_silent(generate(hyp_diff_in_props, type = "permute"))
  expect_error(generate(hyp_chisq_gof, type = "permute"))
  expect_silent(generate(hyp_chisq_ind, type = "permute"))
  expect_error(generate(hyp_mean, type = "permute"))
  expect_silent(generate(hyp_diff_in_means, type = "permute"))
  expect_silent(generate(hyp_anova, type = "permute"))

})

test_that("sensible output", {

  expect_equal(nrow(mtcars) * 500, nrow(generate(hyp_prop, reps = 500, type = "simulate")))
  expect_silent(generate(hyp_mean, reps = 1, type = "bootstrap"))
  expect_error(generate(hyp_mean, reps = 1, type = "other"))
})
