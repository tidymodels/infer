context("calculate")

# calculate arguments
test_that("x is a tibble", {
  vec <- 1:10
  expect_error(calculate(vec, stat = "mean"))
})

test_that("stat argument is appropriate", {
  # stat is a string
  iris_tbl <- tibble::as_tibble(iris)
  expect_error(calculate(iris_tbl, stat = 3))

  # stat is one of the implemented options
  gen_iris_slope <- iris_tbl %>%
    specify(Sepal.Length ~ Sepal.Width) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris_slope, stat = "slopee"))
  expect_error(calculate(gen_iris_slope, stat = "stdev"))
  expect_error(calculate(gen_iris_slope, stat = "stat"))
})

test_that("response attribute has been set", {
  expect_error(calculate(iris, stat = "median"))
})

test_that("variable chosen is of appropriate class (one var problems)", {
  # One sample chisq example
  gen_iris1 <- iris %>%
    specify(Species ~ NULL) %>%
    hypothesize(null = "point",
                p = c("setosa" = .5, "versicolor" = .25, "virginica" = .25)) %>%
    generate(reps = 10, type = "simulate")
  expect_error(calculate(gen_iris1, stat = "mean"))

  # One mean example
  gen_iris_num <- iris %>%
    specify(Sepal.Width ~ NULL) %>%
    hypothesize(null = "point", mu = 3) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris_num, stat = "prop"))

})

test_that("grouping (explanatory) variable is a factor (two var problems)", {
  gen_iris2 <- iris %>%
    specify(Sepal.Width ~ Sepal.Length) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris2, stat = "diff in means"))
  expect_error(calculate(gen_iris2, stat = "diff in medians"))
})

test_that("grouping (explanatory) variable is numeric (two var problems)", {
  gen_iris2a <- iris %>%
    specify(Species ~ Sepal.Length) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris2a, stat = "slope"))
  expect_error(calculate(gen_iris2a, stat = "diff in medians"))
})

test_that("response variable is a factor (two var problems)", {
  gen_iris3 <- iris %>%
    specify(Sepal.Width ~ Species) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris3, stat = "Chisq"))

  # Species has more than 2 levels
  gen_iris4 <- iris %>%
    dplyr::mutate(Sepal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Sepal.Length.Group ~ Species) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris4, stat = "diff in props"))
})

gen_iris5 <- iris %>%
  specify(Species ~ Sepal.Width) %>%
  generate(reps = 10, type = "bootstrap")

test_that("response variable is numeric (two var problems)", {
  expect_error(calculate(gen_iris5, stat = "F"))
})

test_that("properties of tibble passed-in are correct", {
  expect_is(gen_iris5, "grouped_df")
  expect_equal(ncol(gen_iris5), 3)

  gen_iris6 <- iris %>%
    specify(Species ~ NULL) %>%
    generate(reps = 10, type = "bootstrap")
  expect_equal(ncol(gen_iris6), 2)
  expect_error(calculate(gen_iris6))

  # infer class is removed currently after `generate()`
  expect_false("infer" %in% class(gen_iris6))
})

test_that("success is working for diff in means", {
  gen_iris7 <- iris %>%
    dplyr::mutate(Sepal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Sepal.Width ~ Sepal.Length.Group) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_iris7, stat = "diff in means", order = c(">5", "<=5")))
  expect_equal(nrow(calculate(gen_iris7, stat = "diff in means", order = c(">5", "<=5"))), 10)
  expect_equal(ncol(calculate(gen_iris7, stat = "diff in means", order = c(">5", "<=5"))), 2)
})

test_that("chi-square matches chisq.test value", {
  gen_iris8 <- iris %>%
    dplyr::mutate(Petal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Petal.Length.Group ~ Species) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  infer_way <- calculate(gen_iris8, stat = "Chisq")
  #chisq.test way
  trad_way <- gen_iris8 %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(stats::chisq.test(table(.$Petal.Length.Group, .$Species)))) %>%
    dplyr::select(replicate, stat = statistic) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(replicate = as.factor(replicate))
  expect_equal(infer_way, trad_way)
  
  gen_iris9 <- iris %>% 
    specify(Species ~ NULL) %>% 
    hypothesize(null = "point", 
                p = c("setosa" = 1/3, "versicolor" = 1/3, "virginica" = 1/3)) %>% 
    generate(reps = 10, type = "simulate")
  infer_way <- calculate(gen_iris9, stat = "Chisq")
  #chisq.test way
  trad_way <- gen_iris9 %>% 
    dplyr::group_by(replicate) %>% 
    dplyr::do(broom::tidy(stats::chisq.test(table(.$Species)))) %>% 
    dplyr::select(replicate, stat = statistic)
  expect_equal(infer_way, trad_way)
})

test_that("`order` is working", {
  gen_iris10 <- iris %>%
    dplyr::mutate(Petal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Petal.Width ~ Petal.Length.Group) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris10, stat = "diff in means", order = c(TRUE, FALSE)))
  
  gen_iris11 <- iris %>%
    dplyr::mutate(Petal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>% 
    specify(Petal.Width ~ Petal.Length.Group) %>% 
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris11, stat = "diff in medians", order = ">5"))
  expect_error(calculate(gen_iris11, stat = "diff in medians", order = c(NA, ">5")))
  expect_error(calculate(gen_iris11, stat = "diff in medians", order = c(">5", "<=4")))
  expect_error(calculate(gen_iris11, stat = "diff in means", order = c(">5", "<=4", ">4")))
})