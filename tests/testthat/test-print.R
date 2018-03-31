context("print")

test_that("print works", {
  expect_output(print(iris %>%
    specify(Sepal.Length ~ Sepal.Width) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")))
})