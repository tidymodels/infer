test_that("print works", {
  expect_output(print(
    gss_tbl |>
      specify(age ~ hours) |>
      hypothesize(null = "independence") |>
      generate(reps = 10, type = "permute")
  ))
})

test_that("print method fits linewidth with many predictors (#543)", {
  expect_snapshot(specify(mtcars, mpg ~ cyl + disp + hp + drat + wt + qsec))
})
