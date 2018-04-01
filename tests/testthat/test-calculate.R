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
                p = c("setosa" = .5,
                      "versicolor" = .25, "virginica" = .25)) %>%
    generate(reps = 10, type = "simulate")
  expect_error(calculate(gen_iris1, stat = "mean"))

  # One mean example
  gen_iris_num <- iris %>%
    specify(Sepal.Width ~ NULL) %>%
    hypothesize(null = "point", mu = 3) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris_num, stat = "prop"))
  expect_silent(calculate(gen_iris_num, stat = "mean"))
  #expect_error(calculate(gen_iris_num, stat = "median"))
  #expect_error(calculate(gen_iris_num, stat = "sd"))

  gen_iris_num2 <- iris %>%
    specify(Sepal.Width ~ NULL) %>%
    hypothesize(null = "point", med = 3) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris_num2, stat = "prop"))
  #expect_error(calculate(gen_iris_num2, stat = "mean"))
  expect_silent(calculate(gen_iris_num2, stat = "median"))
  #expect_error(calculate(gen_iris_num2, stat = "sd"))

  gen_iris_num3 <- iris %>%
    specify(Sepal.Width ~ NULL) %>%
    hypothesize(null = "point", sigma = 0.6) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris_num3, stat = "prop"))
  #expect_error(calculate(gen_iris_num3, stat = "mean"))
  #expect_error(calculate(gen_iris_num3, stat = "median"))
  expect_silent(calculate(gen_iris_num3, stat = "sd"))
})

test_that("grouping (explanatory) variable is a factor (two var problems)", {
  gen_iris2 <- iris %>%
    specify(Sepal.Width ~ Sepal.Length) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris2, stat = "diff in means"))
  expect_error(calculate(gen_iris2, stat = "diff in medians"))
  # Since shifts to "Slope with t"
  expect_silent(calculate(gen_iris2, stat = "t"))
})

test_that("grouping (explanatory) variable is numeric (two var problems)", {
  gen_iris2a <- iris %>%
    specify(Species ~ Sepal.Length) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris2a, stat = "slope"))
  # Since shifts to "Slope with t"
  expect_error(calculate(gen_iris2a, stat = "t"))
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
    dplyr::mutate(Sepal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Sepal.Length.Group ~ Species, success = ">5") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris4, stat = "diff in props"))

  expect_error(calculate(gen_iris4, stat = "t"))
  
  # Check successful diff in props
  gen_iris4a <- iris %>%
    dplyr::mutate(Sepal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    dplyr::mutate(Sepal.Width.Group =
                    dplyr::if_else(Sepal.Width > 3, "large", "small")) %>%
    specify(Sepal.Length.Group ~ Sepal.Width.Group, success = ">5") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_iris4a, stat = "diff in props",
                          order = c("large", "small")))
  expect_silent(calculate(gen_iris4a, stat = "z",
                          order = c("large", "small")))
  expect_error(calculate(gen_iris4a, stat = "z"))
})

gen_iris5 <- iris %>%
  specify(Species ~ Sepal.Width) %>%
  generate(reps = 10, type = "bootstrap")

test_that("response variable is numeric (two var problems)", {
  expect_error(calculate(gen_iris5, stat = "F"))
})

test_that("two sample mean-type problems are working", {
  gen_iris5a <- iris %>%
    dplyr::mutate(Sepal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>% 
    specify(Sepal.Width ~ Sepal.Length.Group) %>% 
    hypothesize(null = "independence") %>% 
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris5a, stat = "diff in means"))
  expect_silent(calculate(gen_iris5a, stat = "diff in means",
                          order = c(">5", "<=5")))
  expect_error(calculate(gen_iris5a, stat = "t"))
  expect_silent(calculate(gen_iris5a, stat = "t",
                          order = c(">5", "<=5")))
})

test_that("properties of tibble passed-in are correct", {
  expect_is(gen_iris5, "grouped_df")
  expect_equal(ncol(gen_iris5), 3)

  gen_iris6 <- iris %>%
    specify(Species ~ NULL) %>%
    generate(reps = 10, type = "bootstrap")
  expect_equal(ncol(gen_iris6), 2)
  expect_error(calculate(gen_iris6))
})

test_that("order is working for diff in means", {
  gen_iris7 <- iris %>%
    dplyr::mutate(Sepal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Sepal.Width ~ Sepal.Length.Group) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_equal(nrow(calculate(gen_iris7, stat = "diff in means",
                              order = c(">5", "<=5"))), 10)
  expect_equal(ncol(calculate(gen_iris7, stat = "diff in means",
                              order = c(">5", "<=5"))), 2)
})

test_that("chi-square matches chisq.test value", {
  gen_iris8 <- iris %>%
    dplyr::mutate(Petal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Petal.Length.Group ~ Species, success = ">5") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  infer_way <- calculate(gen_iris8, stat = "Chisq")
  #chisq.test way
  trad_way <- gen_iris8 %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(stats::chisq.test(table(.$Petal.Length.Group,
                                                  .$Species)))) %>%
    dplyr::select(replicate, stat = statistic) %>%
    dplyr::ungroup() #%>%
 #   dplyr::mutate(replicate = as.factor(replicate))
  expect_equal(infer_way, trad_way)

  gen_iris9 <- iris %>%
    specify(Species ~ NULL) %>%
    hypothesize(null = "point",
                p = c("setosa" = 1/3,
                      "versicolor" = 1/3,
                      "virginica" = 1/3)) %>%
    generate(reps = 10, type = "simulate")
  infer_way <- calculate(gen_iris9, stat = "Chisq")
  #chisq.test way
  trad_way <- gen_iris9 %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(stats::chisq.test(table(.$Species)))) %>%
    dplyr::select(replicate, stat = statistic)
  expect_equal(infer_way, trad_way)
  
  gen_iris9a <- iris %>%
    specify(Species ~ NULL) %>%
    hypothesize(null = "point",
                p = c("setosa" = 0.8,
                      "versicolor" = 0.1,
                      "virginica" = 0.1)) %>%
    generate(reps = 10, type = "simulate")
  infer_way <- calculate(gen_iris9a, stat = "Chisq")
  #chisq.test way
  trad_way <- gen_iris9a %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(broom::tidy(stats::chisq.test(table(.$Species),
                                            p = c(0.8, 0.1, 0.1)))) %>%
    dplyr::select(replicate, stat = statistic)
  expect_equal(infer_way, trad_way)
  
})

test_that("`order` is working", {
  gen_iris10 <- iris %>%
    dplyr::mutate(Petal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Petal.Width ~ Petal.Length.Group) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_error(calculate(gen_iris10, stat = "diff in means",
                         order = c(TRUE, FALSE)))

  gen_iris11 <- iris %>%
    dplyr::mutate(Petal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Petal.Width ~ Petal.Length.Group) %>%
    generate(reps = 10, type = "bootstrap")
  expect_error(calculate(gen_iris11, stat = "diff in medians",
                         order = ">5"))
  expect_error(calculate(gen_iris11, stat = "diff in medians",
                         order = c(NA, ">5")))
  expect_error(calculate(gen_iris11, stat = "diff in medians",
                         order = c(">5", "<=4")))
  expect_silent(calculate(gen_iris11, stat = "diff in medians",
                         order = c(">5", "<=5")))
  expect_error(calculate(gen_iris11, stat = "diff in means",
                         order = c(">5", "<=4", ">4")))
  # order not given
  expect_error(calculate(gen_iris11, stat = "diff in means"))
  
})

test_that('success is working for stat = "prop"', {
  gen_iris12 <- iris %>%
    dplyr::mutate(Sepal.Length.Group =
                    dplyr::if_else(Sepal.Length > 5, ">5", "<=5")) %>%
    specify(Sepal.Length.Group ~ NULL, success = ">5") %>%
    hypothesize(null = "point", p = 0.3) %>%
    generate(reps = 10, type = "simulate")
  expect_silent(gen_iris12 %>%
                  calculate(stat = "prop"))
  expect_silent(gen_iris12 %>%
                  calculate(stat = "z"))
  
})

test_that("NULL response gives error", {
  iris_improp <- tibble::as_tibble(iris) %>%
    dplyr::select(Sepal.Width, Sepal.Length)

  expect_error(
    iris_improp %>% calculate(stat = "mean")
  )
})

test_that("Permute F test works", {
  gen_iris13 <- iris %>%
    specify(Petal.Width ~ Species) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_iris13, stat = "F"))
})

test_that("Permute slope test works", {
  gen_iris14 <- iris %>%
    specify(Petal.Width ~ Petal.Length) %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_silent(calculate(gen_iris14, stat = "slope"))
})

test_that("order being given when not needed gives warning", {
  gen_iris15 <- iris %>%
    dplyr::mutate(Petal.Length.Group =
                    dplyr::if_else(Sepal.Length > 4, ">4", "<=4")) %>%
    specify(Petal.Length.Group ~ Species, success = ">4") %>%
    hypothesize(null = "independence") %>%
    generate(reps = 10, type = "permute")
  expect_warning(calculate(gen_iris15, stat = "Chisq",
                           order = c("setosa", "virginica")))
})

test_that("warning given if calculate without generate", {
  expect_warning(iris %>% 
                   specify(Species ~ NULL) %>% 
                   hypothesize(null = "point", 
                               p = c("setosa" = 0.4,
                                     "versicolor" = 0.4,
                                     "virginica" = 0.2)) %>% 
                   #generate(reps = 10, type = "simulate") %>% 
                   calculate(stat = "Chisq")
  )
  
})
