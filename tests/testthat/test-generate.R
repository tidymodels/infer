context("generate")

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

  expect_silent(generate(hyp_prop, type = "draw"))
  expect_warning(generate(hyp_diff_in_props, type = "draw"))
  expect_silent(generate(hyp_chisq_gof, type = "draw"))
  expect_warning(generate(hyp_chisq_ind, type = "draw"))
  expect_error(
    expect_warning(generate(hyp_mean, type = "draw"))
  )
  expect_warning(generate(hyp_diff_in_means, type = "draw"))
  expect_warning(generate(hyp_anova, type = "draw"))

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
    nrow(generate(hyp_prop, reps = 500, type = "draw"))
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
  expect_equal(attr(one_prop, "type"), "draw")
  expect_equal(attr(two_props, "type"), "permute")
  expect_equal(attr(gof_chisq, "type"), "draw")
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
      generate(reps = 100, type = "draw")
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
      generate(reps = 100, type = "draw")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ am) %>% # alt: response = mpg, explanatory = am
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap"))

  expect_warning(mtcars_df %>%
      specify(mpg ~ cyl) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "draw")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ hp) %>% # alt: response = mpg, explanatory = cyl
      hypothesize(null = "independence") %>%
      generate(reps = 100, type = "bootstrap")
  )

  expect_warning(mtcars_df %>%
      specify(response = am, success = "1") %>%
      generate(reps = 100, type = "draw")
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
      generate(reps = 100, type = "draw")
  )

  expect_warning(mtcars_df %>%
      specify(mpg ~ hp) %>%
      generate(reps = 100, type = "draw")
  )
})

test_that("mismatches lead to error", {
  expect_error(mtcars_df %>% generate(reps = 10, type = "permute"))
  expect_error(
    mtcars_df %>%
      specify(am ~ NULL, success = "1") %>%
      hypothesize(null = "independence", p = c("1" = 0.5)) %>%
      generate(reps = 100, type = "draw")
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
    'Setting `type = "draw"` in `generate()`.',
    fixed = TRUE
  )
})

test_that("generate() handles `x` response in case of 'simulate' (#299)", {
  expect_named(
    data.frame(x = factor(rbinom(100, size = 1, prob = .5))) %>%
      specify(response = x, success = "1") %>%
      hypothesize(null = "point", p = .5) %>%
      generate(reps = 100, type = "draw"),
    c("x", "replicate")
  )
})

test_that("generate() can permute with multiple explanatory variables", {
  # if the y variable is the one being permuted and the x's
  # are being left alone, then each age + college combination
  # should exist in every replicate
  expect_true(
    gss %>% 
      # add random noise to make the variable truly continuous
      dplyr::mutate(age = age + rnorm(nrow(gss))) %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 3, type = "permute") %>%
      dplyr::ungroup() %>%
      dplyr::count(age, college) %>%
      dplyr::pull(n) %>%
      `==`(3) %>%
      all()
  )
  
  x <- gss %>% 
    specify(hours ~ age + college) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 3, type = "permute")
  
  expect_true(inherits(x, "infer"))
  expect_true(inherits(explanatory_variable(x), "tbl_df"))
  expect_true(inherits(explanatory_name(x), "character"))
  expect_true(inherits(explanatory_expr(x), "call"))
  
  expect_equal(explanatory_name(x), c("age", "college"))
  expect_equal(response_name(x), "hours")
  
  expect_equal(nrow(x), 1500)
  expect_equal(ncol(x), 4)
})

test_that("generate is sensitive to the cols argument", {
  # default argument works appropriately
  expect_equal({ 
      set.seed(1)
      
      gss[1:10,] %>%
        specify(hours ~ age + college) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 2, type = "permute")
      }, { 
      set.seed(1)
      
      gss[1:10,] %>%
        specify(hours ~ age + college) %>%
        hypothesize(null = "independence") %>%
        generate(reps = 2, type = "permute", cols = hours)
  })
  
  # permuting changes output
  expect_silent(
    perm_age <- gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = age)
  )
  
  expect_false(all(perm_age$age[1:10] == perm_age$age[11:20]))
  expect_true(all(perm_age$hours[1:10] == perm_age$hours[11:20]))
  expect_true(all(perm_age$college[1:10] == perm_age$college[11:20]))
  
  expect_silent(
    perm_college <- gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = college)
  )
  
  expect_true(all(perm_college$age[1:10] == perm_college$age[11:20]))
  expect_true(all(perm_college$hours[1:10] == perm_college$hours[11:20]))
  expect_false(all(perm_college$college[1:10] == perm_college$college[11:20]))
  
  expect_silent(
    perm_college_age <- gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = c(college, age))
  )
  
  expect_false(all(perm_college_age$age[1:10] == perm_college_age$age[11:20]))
  expect_true(all(perm_college_age$hours[1:10] == perm_college_age$hours[11:20]))
  expect_false(all(perm_college_age$college[1:10] == perm_college_age$college[11:20]))
})

test_that("cols argument prompts when it ought to", {
  expect_error(
    gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = c(howdy)),
    "column `howdy`.*is not in the supplied data."
  )
  
  expect_error(
    gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = c(howdy, doo)),
    'columns `c\\("howdy", "doo"\\)`.*are not in the supplied data.'
  )
  
  expect_warning(
    gss[1:10,] %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 40) %>%
      generate(reps = 2, type = "bootstrap", cols = c(hours)),
    "is only relevant to.*will be ignored."
  )
  
  expect_error(
    gss[1:10,] %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 2, type = "permute", cols = "hours"),
    'unquoted variable names'
  )
})

test_that("type = 'draw'/'simulate' superseding handled gracefully", {
  # message on type = 'simulate'
  expect_message(
    mtcars_df %>%
      specify(response = am, success = "1") %>%
      hypothesize(null = "point", p = .5) %>%
      generate(type = "simulate"),
    '`"simulate"` generation type.*renamed to `"draw"`.*quiet'
  )
  
  # don't message on type = 'draw'
  expect_silent(
    mtcars_df %>%
      specify(response = am, success = "1") %>%
      hypothesize(null = "point", p = .5) %>%
      generate(type = "draw")
  )

  # mention new generation types when supplied a bad one
  expect_error(
    mtcars_df %>%
      specify(response = am, success = "1") %>%
      hypothesize(null = "point", p = .5) %>%
      generate(type = "boop"),
    'should be one of "bootstrap", "permute", or "draw"'
  )
  
  # warns with either alias when given unexpected generate type
  expect_error(
    expect_warning(
      mtcars_df %>%
        specify(response = mpg) %>%
        hypothesize(null = "point", mu = 20) %>%
        generate(type = "draw"),
      'have given `type = "draw"`.*expected to be `"bootstrap"`.*untested'
    )
  )
  
  expect_error(
    expect_warning(
      mtcars_df %>%
        specify(response = mpg) %>%
        hypothesize(null = "point", mu = 20) %>%
        generate(type = "draw"),
      'have given `type = "draw"`.*expected to be `"bootstrap"`.*untested'
    )
  )
  
  expect_equivalent(
    {
      set.seed(1)
      
      expect_message(
        mtcars_df %>%
          specify(response = am, success = "1") %>%
          hypothesize(null = "point", p = .5) %>%
          generate(type = "simulate")
      )
    }, {
      set.seed(1)
      
      mtcars_df %>%
        specify(response = am, success = "1") %>%
        hypothesize(null = "point", p = .5) %>%
        generate(type = "draw")
    }
  )
})
