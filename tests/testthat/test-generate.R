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

  expect_equal(nrow(mtcars) * 500,
               nrow(generate(hyp_prop, reps = 500, type = "simulate")))
  expect_silent(generate(hyp_mean, reps = 1, type = "bootstrap"))
  expect_error(generate(hyp_mean, reps = 1, type = "other"))
})

test_that("auto `type` works (generate)", {
  mtcars <- as.data.frame(mtcars) %>%
    dplyr::mutate(cyl = factor(cyl),
                  vs = factor(vs),
                  am = factor(am),
                  gear = factor(gear),
                  carb = factor(carb))

  one_mean <- mtcars %>%
    specify(response = mpg) %>% # formula alt: mpg ~ NULL
    hypothesize(null = "point", mu = 25) %>%
    generate(reps = 100)

  one_nonshift_mean <- mtcars %>%
    specify(response = mpg) %>%
    generate(reps = 100)

  one_median <- mtcars %>%
    specify(response = mpg) %>% # formula alt: mpg ~ NULL
    hypothesize(null = "point", med = 26) %>%
    generate(reps = 100)

  one_prop <- mtcars %>%
    specify(response = am, success = "1") %>% # formula alt: am ~ NULL
    hypothesize(null = "point", p = .25) %>%
    generate(reps = 100)

  two_props <- mtcars %>%
    specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  gof_chisq <- mtcars %>%
    specify(cyl ~ NULL) %>% # alt: response = cyl
    hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
    generate(reps = 100)

  indep_chisq <- mtcars %>%
    specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  two_means <- mtcars %>%
    specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  anova_f <- mtcars %>%
    specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  slopes <- mtcars %>%
    specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  one_nonshift_prop <- mtcars %>%
    specify(response = am, success = "1") %>%
    generate(reps = 100)

  two_means_boot <- mtcars %>%
    specify(mpg ~ am) %>%
    generate(reps = 100)

  two_props_boot <- mtcars %>%
    specify(am ~ vs, success = "1") %>%
    generate(reps = 100)

  slope_boot <- mtcars %>%
    specify(mpg ~ hp) %>%
    generate(reps = 100)

    expect_equal(attr(one_mean, "type"), "bootstrap")
    expect_equal(attr(one_nonshift_mean, "type"), "bootstrap")
    expect_equal(attr(one_median, "type"), "bootstrap")
    expect_equal(attr(one_prop, "type"), "simulate")
    expect_equal(attr(two_props, "type"), "permute")
    expect_equal(attr(gof_chisq, "type"), "simulate")
    expect_equal(attr(indep_chisq, "type"), "permute")
    expect_equal(attr(two_means, "type"), "permute")
    expect_equal(attr(anova_f, "type"), "permute")
    expect_equal(attr(slopes, "type"), "permute")
    expect_equal(attr(one_nonshift_prop, "type"), "bootstrap")
    expect_equal(attr(two_means_boot, "type"), "bootstrap")
    expect_equal(attr(two_props_boot, "type"), "bootstrap")
    expect_equal(attr(slope_boot, "type"), "bootstrap")

    expect_error(mtcars %>%
      specify(response = mpg) %>% # formula alt: mpg ~ NULL
      hypothesize(null = "point", mu = 25) %>%
      generate(reps = 100, type = "permute"))

    expect_error(mtcars %>%
      specify(response = mpg) %>%
      generate(reps = 100, type = "simulate"))

    expect_error(mtcars %>%
      specify(response = mpg) %>% # formula alt: mpg ~ NULL
      hypothesize(null = "point", med = 26) %>%
      generate(reps = 100, type = "permute"))

    expect_error(mtcars %>%
      specify(response = am, success = "1") %>% # formula alt: am ~ NULL
      hypothesize(null = "point", p = .25) %>%
      generate(reps = 100, type = "bootstrap"))

    expect_error(mtcars %>%
      specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap"))

    expect_error(mtcars %>%
      specify(cyl ~ NULL) %>% # alt: response = cyl
      hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
      generate(reps = 100, type = "bootstrap"))

    expect_error(mtcars %>%
      specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "simulate"))

    expect_error(mtcars %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap"))

    expect_error(mtcars %>%
      specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "simulate"))

    expect_error(mtcars %>%
      specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap"))

    expect_error(mtcars %>%
      specify(response = am, success = "1") %>%
      generate(reps = 100, type = "simulate"))

    expect_error(mtcars %>%
      specify(mpg ~ am) %>%
      generate(reps = 100, type = "permute"))

    expect_error(mtcars %>%
      specify(am ~ vs, success = "1") %>%
      generate(reps = 100, type = "simulate"))

    expect_error(mtcars %>%
      specify(mpg ~ hp) %>%
      generate(reps = 100, type = "simulate"))

})

test_that("mismatches lead to error", {
  expect_error(mtcars %>% generate(reps = 10, type = "permute"))
  expect_error(mtcars %>% specify(am ~ NULL, success = "1") %>%
                 hypothesize(null = "independence", p = c("1" = 0.5)) %>%
                 generate(reps = 100, type = "simulate"))
  expect_error(mtcars %>%
                 specify(cyl ~ NULL) %>% # alt: response = cyl
                 hypothesize(null = "point", p = c("4" = .5, "6" = .25,
                                                   "8" = .25)) %>%
                 generate(reps = 100, type = "bootstrap"))
  expect_error(mtcars %>%
                 specify(mpg ~ hp) %>%
                 generate(reps = 100, type = "other"))
})

test_that("generate() handles `NULL` value of `type`", {
  expect_error(generate(hyp_prop, type = NULL), "NULL.*type")
})
