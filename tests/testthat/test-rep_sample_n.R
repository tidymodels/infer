context("rep_sample_n")

N <- 5
population <- tibble::tibble(
  ball_id = 1:N,
  color = factor(c(rep("red", 3), rep("white", N - 3)))
)

test_that("rep_sample_n is sensitive to the size argument", {
  set.seed(1)
  reps <- 10
  s1 <- 2
  s2 <- 3
  
  res1 <- population %>% rep_sample_n(size = s1, reps = reps)
  res2 <- population %>% rep_sample_n(size = s2, reps = reps)

  expect_equal(ncol(res1), ncol(res2))
  expect_equal(ncol(res1), 3)
  
  expect_equal(nrow(res1) / s1, nrow(res2) / s2)
  expect_equal(nrow(res1), reps * s1)
})

test_that("rep_sample_n is sensitive to the reps argument", {
  set.seed(1)
  r1 <- 10
  r2 <- 5
  size <- 2
  
  res1 <- population %>% rep_sample_n(size = size, reps = r1)
  res2 <- population %>% rep_sample_n(size = size, reps = r2)
  
  expect_equal(ncol(res1), ncol(res2))
  expect_equal(ncol(res1), 3)
  
  expect_equal(nrow(res1) / r1, nrow(res2) / r2)
  expect_equal(nrow(res1), r1 * size)
})

test_that("rep_sample_n is sensitive to the replace argument", {
  set.seed(1)
  res1 <- population %>% rep_sample_n(size = 5, reps = 100, replace = TRUE)
  
  set.seed(1)
  res2 <- population %>% rep_sample_n(size = 5, reps = 100, replace = FALSE)
  
  expect_true(all(res1$replicate == res2$replicate))
  expect_false(all(res1$ball_id == res2$ball_id))
  expect_false(all(res1$color == res2$color))
  
  expect_equal(ncol(res1), ncol(res2))
  expect_equal(ncol(res1), 3)
})

test_that("rep_sample_n is sensitive to the prob argument", {
  set.seed(1)
  res1 <- population %>% 
    rep_sample_n(
      size = 5, 
      reps = 100, 
      replace = TRUE,
      prob = c(1, rep(0, 4))
    )
  
  expect_true(all(res1$ball_id == 1))
  expect_true(all(res1$color == "red"))
})

test_that("rep_sample_n errors with bad arguments", {
  expect_error(
    population %>%
      rep_sample_n(size = 2, reps = 10, prob = rep(x = 1/5, times = 100))
  )
  
  expect_error(
    population %>%
      rep_sample_n(size = 2, reps = 10, prob = c(1/2, 1/2))
  )
  
  expect_error(
    population %>%
      rep_sample_n(size = "a lot", reps = 10)
  )
  
  expect_error(
    population %>%
      rep_sample_n(size = 2, reps = "a lot")
  )
})

test_that("rep_slice_sample works", {
  set.seed(1)
  res1 <- rep_sample_n(population, size = 2, reps = 5, prob = rep(1/N, N))
  
  set.seed(1)
  res2 <- rep_slice_sample(population, n = 2, reps = 5, weight_by = rep(1/N, N))
  
  expect_equal(res1, res2)
})
