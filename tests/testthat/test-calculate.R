context("calculate")

iris_tbl <- tibble::as_tibble(iris)
gen_iris_slope <- iris_tbl %>% 
  specify(Sepal.Length ~ Sepal.Width) %>% 
  hypothesize(null = "independence") %>% 
  generate(reps = 10, type = "permute")

# calculate arguments
test_that("x is a tibble", {
  vec <- 1:10
  expect_error(calculate(vec, stat = "mean"))
})

test_that("stat argument is appropriate", {
  # stat is a string
  expect_error(calculate(iris_tbl, stat = 3))
  # stat is one of the implemented options
  expect_error(calculate(gen_iris_slope, stat = "meann"))
  expect_error(calculate(gen_iris_slope, stat = "stdev"))
  expect_error(calculate(gen_iris_slope, stat = "stat"))
})

test_that("response attribute has been set", {
  expect_error(calculate(iris_tbl, stat = "median"))
})

test_that("variable chosen of appropriate class", {
  # One sample chisq example
  gen_iris_one <- iris %>% 
    specify(Species ~ NULL) %>% 
    hypothesize(null = "point", 
                p = c("setosa" = .5, "versicolor" = .25, "virginica" = .25)) %>% 
    generate(reps = 10, type = "simulate")
  expect_error(calculate(gen_iris_one, stat = "mean"))
  
  gen_iris_num <- iris %>% 
    specify(Sepal.Width ~ NULL) %>% 
    hypothesize(null = "point", mu = 3) %>% 
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris_num, stat = "prop"))
  
})