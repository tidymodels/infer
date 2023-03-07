test_that("observe() output is equal to core verbs", {
  expect_equal(
    gss %>%
      observe(hours ~ NULL, stat = "mean"),
    gss %>%
      specify(hours ~ NULL) %>%
      calculate(stat = "mean")
  )

  expect_equal(
    gss %>%
      observe(hours ~ NULL, stat = "t", null = "point", mu = 40),
    gss %>%
      specify(hours ~ NULL) %>%
      hypothesize(null = "point", mu = 40) %>%
      calculate(stat = "t")
  )

  expect_equal(
    observe(
      gss,
      age ~ college,
      stat = "diff in means",
      order = c("degree", "no degree")
    ),
    gss %>%
      specify(age ~ college) %>%
      calculate("diff in means", order = c("degree", "no degree"))
  )
})

test_that("observe messages/warns/errors informatively", {
  expect_equal(
    capture.output(
      gss %>%
        observe(hours ~ NULL, stat = "mean", mu = 40),
      type = "message"
    ),
    capture.output(
      gss %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 40) %>%
        calculate(stat = "mean"),
      type = "message"
    ),
  )

  expect_warning(
    expect_equal(
      capture.output(
        gss %>%
          observe(hours ~ NULL, stat = "t"),
        type = "message"
      ),
      capture.output(
        gss %>%
          specify(hours ~ NULL) %>%
          calculate(stat = "t"),
        type = "message"
      ),
    )
  )

  expect_error(
    expect_equal(
      capture.output(
        gss %>%
          observe(hours ~ age, stat = "diff in means"),
        type = "message"
      ),
      capture.output(
        gss %>%
          specify(hours ~ age) %>%
          calculate(stat = "diff in means"),
        type = "message"
      ),
    )
  )

  expect_error(
    expect_equal(
      gss %>%
        observe(explanatory = age, stat = "diff in means"),
      gss %>%
        specify(explanatory = age) %>%
        calculate(stat = "diff in means")
    )
  )
})

test_that("observe() works with either specify() interface", {
  # unnamed formula argument
  expect_equivalent(
    gss %>%
      observe(hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean")
  )

  expect_equal(
    gss %>%
      observe(
        hours ~ college,
        stat = "diff in means",
        order = c("degree", "no degree")
      ),
    gss %>%
      specify(hours ~ college) %>%
      calculate(stat = "diff in means", order = c("degree", "no degree"))
  )

  # named formula argument
  expect_equivalent(
    gss %>%
      observe(formula = hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean")
  )

  expect_equivalent(
    gss %>%
      observe(formula = hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean")
  )

  expect_equal(
    gss %>%
      observe(
        formula = hours ~ college,
        stat = "diff in means",
        order = c("degree", "no degree")
      ),
    gss %>%
      specify(formula = hours ~ college) %>%
      calculate(stat = "diff in means", order = c("degree", "no degree"))
  )
})

test_that("observe() output is the same as the old wrappers", {
  expect_equal(
    gss_tbl %>%
      observe(college ~ partyid, stat = "Chisq") %>%
      dplyr::pull(),
    expect_warning(
      gss_tbl %>%
        chisq_stat(college ~ partyid)
    )
  )

  expect_equal(
    gss_tbl %>%
      observe(stat = "t", hours ~ sex, order = c("male", "female")) %>%
      dplyr::pull(),
    expect_warning(
      gss_tbl %>%
        t_stat(hours ~ sex, order = c("male", "female"))
    )
  )
})

