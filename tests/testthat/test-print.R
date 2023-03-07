test_that("print works", {
  expect_output(print(
    gss_tbl %>%
      specify(age ~ hours) %>%
      hypothesize(null = "independence") %>%
      generate(reps = 10, type = "permute")
  ))
})
