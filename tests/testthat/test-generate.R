context("generate")

source(test_path("test-aaa-helper-data.R"))

hyp_prop <- mtcars_df %>%
  specify(response = am, success = "1") %>%
  hypothesize(null = "point", p = .5)

hyp_diff_in_props <- mtcars_df %>%
  specify(am ~ vs, success = "1") %>%
  hypothesize(null = "independence")

hyp_chisq_gof <- mtcars_df %>%
  specify(response = cyl) %>%
  hypothesize(null = "point", p = c("4" = 1/3, "6" = 1/3, "8" = 1/3))

hyp_chisq_ind <- mtcars_df %>%
  specify(cyl ~ vs) %>%
  hypothesize(null = "independence")

hyp_mean <- mtcars_df %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", mu = 3)

hyp_median <- mtcars_df %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", med = 3)

hyp_sd <- mtcars_df %>%
  specify(response = mpg) %>%
  hypothesize(null = "point", sigma = 7)

hyp_diff_in_means <- mtcars_df %>%
  specify(mpg ~ vs) %>%
  hypothesize(null = "independence")

hyp_anova <- mtcars_df %>%
  specify(mpg ~ cyl) %>%
  hypothesize(null = "independence")

test_that("cohesion with type argument", {
  expect_warning(generate(hyp_prop, type = "bootstrap"))
  expect_warning(generate(hyp_diff_in_props, type = "bootstrap"))
  expect_warning(generate(hyp_chisq_gof, type = "bootstrap"))
  expect_warning(generate(hyp_chisq_ind, type = "bootstrap"))
  expect_silent(generate(hyp_mean, type = "bootstrap"))
  expect_silent(generate(hyp_median, type = "bootstrap"))
  expect_silent(generate(hyp_sd, type = "bootstrap"))
  expect_warning(generate(hyp_diff_in_means, type = "bootstrap"))
  expect_warning(generate(hyp_anova, type = "bootstrap"))

  expect_silent(generate(hyp_prop, type = "simulate"))
  expect_warning(generate(hyp_diff_in_props, type = "simulate"))
  expect_silent(generate(hyp_chisq_gof, type = "simulate"))
  expect_warning(generate(hyp_chisq_ind, type = "simulate"))
  expect_error(
    expect_warning(generate(hyp_mean, type = "simulate"))
  )
  expect_warning(generate(hyp_diff_in_means, type = "simulate"))
  expect_warning(generate(hyp_anova, type = "simulate"))

  expect_error(
    expect_warning(generate(hyp_prop, type = "permute"))
  )
  expect_silent(generate(hyp_diff_in_props, type = "permute"))
  expect_error(
    expect_warning(generate(hyp_chisq_gof, type = "permute"))
  )
  expect_silent(generate(hyp_chisq_ind, type = "permute"))
  expect_error(
    expect_warning(generate(hyp_mean, type = "permute"))
  )
  expect_silent(generate(hyp_diff_in_means, type = "permute"))
  expect_silent(generate(hyp_anova, type = "permute"))
})

test_that("sensible output", {
  expect_equal(
    nrow(mtcars_df) * 500,
    nrow(generate(hyp_prop, reps = 500, type = "simulate"))
  )
  expect_silent(generate(hyp_mean, reps = 1, type = "bootstrap"))
  expect_error(generate(hyp_mean, reps = 1, type = "other"))
  expect_equal(class(generate(hyp_mean, type = "bootstrap"))[1], "infer")
})

test_that("auto `type` works (generate)", {
  one_mean <- mtcars_df %>%
    specify(response = mpg) %>% # formula alt: mpg ~ NULL
    hypothesize(null = "point", mu = 25) %>%
    generate(reps = 100)

  one_nonshift_mean <- mtcars_df %>%
    specify(response = mpg) %>%
    generate(reps = 100)

  one_median <- mtcars_df %>%
    specify(response = mpg) %>% # formula alt: mpg ~ NULL
    hypothesize(null = "point", med = 26) %>%
    generate(reps = 100)

  one_prop <- mtcars_df %>%
    specify(response = am, success = "1") %>% # formula alt: am ~ NULL
    hypothesize(null = "point", p = .25) %>%
    generate(reps = 100)

  two_props <- mtcars_df %>%
    specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  gof_chisq <- mtcars_df %>%
    specify(cyl ~ NULL) %>% # alt: response = cyl
    hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
    generate(reps = 100)

  indep_chisq <- mtcars_df %>%
    specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  two_means <- mtcars_df %>%
    specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  anova_f <- mtcars_df %>%
    specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  slopes <- mtcars_df %>%
    specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
    hypothesize(null = "independence") %>%
    generate(reps = 100)

  one_nonshift_prop <- mtcars_df %>%
    specify(response = am, success = "1") %>%
    generate(reps = 100)

  two_means_boot <- mtcars_df %>%
    specify(mpg ~ am) %>%
    generate(reps = 100)

  two_props_boot <- mtcars_df %>%
    specify(am ~ vs, success = "1") %>%
    generate(reps = 100)

  slope_boot <- mtcars_df %>%
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

  expect_error(
    expect_warning(mtcars_df %>%
      specify(response = mpg) %>% # formula alt: mpg ~ NULL
      hypothesize(null = "point", mu = 25) %>%
      generate(reps = 100, type = "permute")
    )
  )

  expect_warning(mtcars_df %>%
      specify(response = mpg) %>%
      generate(reps = 100, type = "simulate")
  )

  expect_warning(
    expect_error(mtcars_df %>%
        specify(response = mpg) %>% # formula alt: mpg ~ NULL
        hypothesize(null = "point", med = 26) %>%
        generate(reps = 100, type = "permute")
    )
  )

  expect_warning(mtcars_df %>%
      specify(response = am, success = "1") %>% # formula alt: am ~ NULL
      hypothesize(null = "point", p = .25) %>%
      generate(reps = 100, type = "bootstrap")
  )

  expect_warning(mtcars_df %>%
      specify(am ~ vs, success = "1") %>% # alt: response = am, explanatory = vs
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap")
  )

  expect_warning(mtcars_df %>%
      specify(cyl ~ NULL) %>% # alt: response = cyl
      hypothesize(null = "point", p = c("4" = .5, "6" = .25, "8" = .25)) %>%
      generate(reps = 100, type = "bootstrap")
  )

  expect_warning(mtcars_df %>%
      specify(cyl ~ am) %>% # alt: response = cyl, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "simulate")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap"))

  expect_warning(mtcars_df %>%
      specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "simulate")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap")
  )

  expect_warning(mtcars_df %>%
      specify(response = am, success = "1") %>%
      generate(reps = 100, type = "simulate")
  )

  expect_error(
    expect_warning(
      mtcars_df %>%
        specify(mpg ~ am) %>%
        generate(reps = 100, type = "permute")
    ),
    "independence hypothesis test"
  )

  expect_warning(mtcars_df %>%
      specify(am ~ vs, success = "1") %>%
      generate(reps = 100, type = "simulate")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ hp) %>%
      generate(reps = 100, type = "simulate")
  )
})

test_that("mismatches lead to error", {
  expect_error(mtcars_df %>% generate(reps = 10, type = "permute"))
  expect_error(
    mtcars_df %>%
      specify(am ~ NULL, success = "1") %>%
      hypothesize(null = "independence", p = c("1" = 0.5)) %>%
      generate(reps = 100, type = "simulate")
  )
  expect_warning(
    mtcars_df %>%
      specify(cyl ~ NULL) %>% # alt: response = cyl
      hypothesize(
        null = "point", p = c("4" = .5, "6" = .25, "8" = .25)
      ) %>%
      generate(reps = 100, type = "bootstrap"))
  expect_error(
    mtcars_df %>% specify(mpg ~ hp) %>% generate(reps = 100, type = "other")
  )
})

test_that("generate() handles `NULL` value of `type`", {
  expect_message(
    generate(hyp_prop, type = NULL),
    'Setting `type = "simulate"` in `generate()`.',
    fixed = TRUE
  )
})

test_that("generate() handles `x` response in case of 'simulate' (#299)", {
  expect_named(
    data.frame(x = factor(rbinom(100, size = 1, prob = .5))) %>%
      specify(response = x, success = "1") %>%
      hypothesize(null = "point", p = .5) %>%
      generate(reps = 100, type = "simulate"),
    c("x", "replicate")
  )
})
