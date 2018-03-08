context("hypothesize")

mtcars <- as.data.frame(mtcars) %>%
  dplyr::mutate(cyl = factor(cyl),
         vs = factor(vs),
         am = factor(am),
         gear = factor(gear),
         carb = factor(carb))

test_that("hypothesize arguments function",{
  
  mtcars_f <- dplyr::mutate(mtcars, cyl = factor(cyl))
  mtcars_s <- mtcars_f %>% specify(response = mpg)
  matrix1 <- matrix(data = NA, nrow = 3, ncol = 3)
  
  expect_error(hypothesize(matrix1))
  expect_error(hypothesize(mtcars_s, null = NA))
  expect_error(hypothesize(mtcars_s))
  
  expect_error(mtcars_s %>% hypothesize(null = "point", mean = 3))
  
  expect_error(mtcars_s %>% hypothesize(null = "independence"))
  expect_error(mtcars_s %>% hypothesize(null = "point"))
  expect_warning(mtcars_s %>% 
                   hypothesize(null = c("point", "independence"), mu = 3))
  
  expect_error(mtcars %>% dplyr::select(vs) %>%
                 hypothesize(null = "point", mu = 1))
  
  expect_error(mtcars %>% specify(response = vs) %>% 
                 hypothesize(null = "point", mu = 1))
  
  expect_error(mtcars %>% specify(response = vs) %>% 
                 hypothesize(null = "point", p = 1.1))
  expect_error(mtcars %>% specify(response = vs) %>% 
                 hypothesize(null = "point", p = -23))
  
  expect_error(mtcars_s %>% 
                 hypothesize(null = "point", 
                             p = c("4" = .2, "6" = .25, "8" = .25)))
  
  expect_error(mtcars_s %>% hypothesize(null = "point", p = 0.2))
  expect_warning(mtcars %>% specify(mpg ~ vs) %>%
                   hypothesize(null = "independence", p = 0.5))
  
  expect_error(mtcars_s %>% hypothesize())
})
