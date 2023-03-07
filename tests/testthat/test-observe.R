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
      calculate("diff in means", order = c("degree", "no degree")),
    ignore_attr = TRUE
  )
})

test_that("observe messages/warns/errors informatively", {
  expect_equal(
    expect_message(
      gss %>%
        observe(hours ~ NULL, stat = "mean", mu = 40)
    ) %>% conditionMessage(),
    expect_message(
      gss %>%
        specify(hours ~ NULL) %>%
        hypothesize(null = "point", mu = 40) %>%
        calculate(stat = "mean")
    ) %>% conditionMessage()
  )

  expect_equal(
     expect_warning(
        gss %>%
           observe(hours ~ NULL, stat = "t")
     ) %>% conditionMessage(),
     expect_warning(
        gss %>%
           specify(hours ~ NULL) %>%
           calculate(stat = "t")
     ) %>% conditionMessage()
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
  expect_equal(
    gss %>%
      observe(hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean"),
    ignore_attr = TRUE
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
  expect_equal(
    gss %>%
      observe(formula = hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean"),
    ignore_attr = TRUE
  )

  expect_equal(
    gss %>%
      observe(formula = hours ~ NULL, stat = "mean"),
    gss %>%
      observe(response = hours, stat = "mean"),
    ignore_attr = TRUE
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
   expect_warning(
      res_wrap <- gss_tbl %>%
         chisq_stat(college ~ partyid)
   )

   expect_equal(
    gss_tbl %>%
      observe(college ~ partyid, stat = "Chisq") %>%
      dplyr::pull(),
    res_wrap
  )

  expect_warning(
    res_wrap_2 <- gss_tbl %>%
       t_stat(hours ~ sex, order = c("male", "female"))
  )

  expect_equal(
    gss_tbl %>%
      observe(stat = "t", hours ~ sex, order = c("male", "female")) %>%
      dplyr::pull(),
    res_wrap_2
  )
})

