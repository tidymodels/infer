context("rep_sample_n")

N <- 2400
population <- tibble::data_frame(
  ball_ID = 1:N,
  color = c(rep("red", 900), rep("white", N - 900))
)

# calculate arguments
test_that("x is a tibble", {
  expect_silent(population %>%
                  rep_sample_n(size = 50, reps = 10))
})

