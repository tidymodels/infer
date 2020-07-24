context("rep_sample_n")

N <- 5
population <- tibble::tibble(
  ball_id = 1:N,
  color = factor(c(rep("red", 3), rep("white", N - 3)))
)

test_that("rep_sample_n works", {
  expect_silent(test_rep <- population %>% rep_sample_n(size = 2, reps = 10))
  
  expect_error(
    population %>%
      rep_sample_n(size = 2, reps = 10, prob = rep(x = 1/5, times = 100))
  )
  
  expect_error(
    population %>%
      rep_sample_n(size = 2, reps = 10, prob = c(1/2, 1/2))
  )
  
  expect_equal(c("replicate", names(population)), names(test_rep))
})

test_that("rep_slice_sample works", {
  
})
