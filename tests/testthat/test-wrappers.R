context("wrappers")

iris2 <- iris %>%
  dplyr::filter(Species != "setosa")

iris3 <- iris %>%
  dplyr::mutate(Sepal.Length.Group = 
                  dplyr::if_else(Sepal.Length > 5, ">5", "<=5"))

test_that("t_test works", {
  expect_silent(iris2 %>% t_test(Sepal.Width ~ Species))
  expect_error(iris2 %>% t_test(response = "Sepal.Width",
                                explanatory = "Species"))
## Not implemented
#  expect_silent(iris2 %>% t_test(response = Sepal.Width,
#                                 explanatory = Species))
})

test_that("chisq_test works", {
  expect_silent(iris3 %>% chisq_test(Sepal.Length.Group ~ Species))
  new_way <- iris3 %>% chisq_test(Sepal.Length.Group ~ Species)
  old_way <- chisq.test(x = table(iris3$Species,
                                  iris3$Sepal.Length.Group)) %>% 
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
  
  expect_equal(new_way, old_way)
  ## Not implemented
  #  expect_silent(iris3 %>% chisq_test(response = Sepal.Length.Group,
  #                                 explanatory = Species))
})

test_that("_stat functions work", {
  expect_silent(iris3 %>% chisq_stat(Sepal.Length.Group ~ Species))
  expect_silent(iris2 %>% t_stat(Sepal.Width ~ Species))
})