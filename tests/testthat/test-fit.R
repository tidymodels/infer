x1 <- gss[1:100,] %>% specify(response = hours)
x2 <- gss[1:100,] %>% specify(hours ~ NULL)
x3 <- gss[1:100,] %>% specify(response = hours, explanatory = c(age, college))
x4 <- gss[1:100,] %>% specify(hours ~ age + college)

test_that("get_formula helper works", {
  expect_false(has_attr(x1, "formula"))
  expect_true(has_attr( x2, "formula"))
  expect_false(has_attr(x3, "formula"))
  expect_true(has_attr( x4, "formula"))

  expect_equal(get_formula(x1), get_formula(x2), ignore_attr = TRUE)
  expect_equal(get_formula(x3), get_formula(x4), ignore_attr = TRUE)
})

test_that("fit_linear_model helper works", {
  x3_m <-
    fit_linear_model(
      x3,
      get_formula(x3)
    )

  x4_m <-
    fit_linear_model(
      x3,
      get_formula(x3)
    )

  expect_equal(x3_m, x4_m)
  expect_equal(nrow(x3_m), 3)
  expect_equal(ncol(x3_m), 2)

  expect_equal(
    c("term", "estimate"),
    colnames(x3_m)
  )

  expect_equal(
    c("character", "numeric"),
    purrr::map_chr(x3_m, class) %>% unname()
  )

  expect_equal(
    c("intercept", "age", "collegedegree"),
    x3_m$term
  )
})

test_that("fit.infer can handle generated objects", {
  x3_fit <- x3 %>% fit()

  x3_gen_fit <- x3 %>%
    hypothesize(null = 'independence') %>%
    generate(reps = 2, type = "permute")%>%
    fit()

  expect_equal(unique(x3_fit$term), unique(x3_gen_fit$term))
  expect_equal(nrow(x3_fit) * 2, nrow(x3_gen_fit))
  expect_equal(ncol(x3_fit) + 1, ncol(x3_gen_fit))
  expect_equal(length(unique(x3_gen_fit$replicate)), 2)
  expect_equal(
    colnames(x3_fit),
    colnames(x3_gen_fit)[colnames(x3_gen_fit) != "replicate"]
  )
})

test_that("fit.infer messages informatively on excessive null", {
  expect_snapshot(
    gss %>%
      specify(hours ~ age + college) %>%
      hypothesize(null = "independence") %>%
      fit()
  )

  expect_silent(
    gss %>%
      specify(hours ~ age + college) %>%
      fit()
  )
})

test_that("fit.infer logistic regression works", {
  # linear regression default works
  expect_equal(
    gss %>%
      specify(hours ~ age + college) %>%
      fit(),
    gss %>%
      specify(hours ~ age + college) %>%
      fit(family = stats::gaussian)
  )

  # logistic regression default works
  expect_equal(
    gss %>%
      specify(college ~ age + hours) %>%
      fit(family = stats::binomial),
    gss %>%
      specify(college ~ age + hours) %>%
      fit()
  )

  # errors informatively with multinomial response variable
  expect_snapshot(error = TRUE,
    gss %>%
      specify(finrela ~ age + college) %>%
      fit()
  )

  # works as expected for `generate()`d objects
  fit_gen <- gss %>%
    specify(college ~ age + hours) %>%
    hypothesize(null = "independence") %>%
    generate(type = "permute", reps = 2) %>%
    fit()

  fit_obs <- gss %>%
    specify(college ~ age + hours) %>%
    fit()

  expect_equal(nrow(fit_gen), nrow(fit_obs) * 2)
  expect_equal(ncol(fit_gen), ncol(fit_obs) + 1)

  # responds to success argument
  fit_deg <- gss %>%
    specify(college ~ age + hours, success = "degree") %>%
    fit()

  fit_no_deg <- gss %>%
    specify(college ~ age + hours, success = "no degree") %>%
    fit()

  expect_equal(fit_deg$term, fit_no_deg$term)
  expect_equal(fit_deg$estimate, -fit_no_deg$estimate)
})
