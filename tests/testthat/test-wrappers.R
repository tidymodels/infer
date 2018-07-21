context("wrappers")

iris2 <- iris %>%
  dplyr::filter(Species != "setosa") %>% 
  droplevels(.$Species)

iris3 <- iris %>%
  dplyr::mutate(Sepal.Length.Group = 
                  dplyr::if_else(Sepal.Length > 5, ">5", "<=5"))

test_that("t_test works", {
  # order is missing
  expect_error(iris2 %>% t_test(Sepal.Width ~ Species))
  
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
  # Test of independence
  expect_silent(
    iris3 %>% chisq_stat(Sepal.Length.Group ~ Species)
    )
  another_way <- iris3 %>%
    chisq_test(Sepal.Length.Group ~ Species) %>%
    dplyr::select(statistic)
  obs_stat_way <- iris3 %>% 
    chisq_stat(Sepal.Length.Group ~ Species)
  one_more <- chisq.test(
    table(iris3$Species, 
                iris3$Sepal.Length.Group)
    )$statistic

  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(one_more, dplyr::pull(obs_stat_way))
  
  # Goodness of Fit
  expect_error(iris3 %>% chisq_test(Species ~ NULL))
  expect_error(iris3 %>% chisq_stat(Species ~ NULL))
#  another_way <- iris3 %>%
#    chisq_test(Species ~ NULL) %>%
#    dplyr::select(statistic)
#  obs_stat_way <- iris3 %>% 
#    chisq_stat(Species ~ NULL)
#  expect_equivalent(another_way, obs_stat_way)
  
  # Two sample t
  expect_silent(
    iris2 %>% t_stat(Sepal.Width ~ Species, 
                                 order = c("virginica", "versicolor"))
    )
  another_way <- iris2 %>%
    t_test(Sepal.Width ~ Species, order = c("virginica", "versicolor")) %>%
    dplyr::select(statistic)
  obs_stat_way <- iris2 %>% 
    t_stat(Sepal.Width ~ Species, order = c("virginica", "versicolor"))
  expect_equivalent(another_way, obs_stat_way)
  
  # One sample t
  expect_silent(iris2 %>% t_stat(Sepal.Width ~ NULL))
  another_way <- iris2 %>%
    t_test(Sepal.Width ~ NULL) %>%
    dplyr::select(statistic)
  obs_stat_way <- iris2 %>% 
    t_stat(Sepal.Width ~ NULL)
  expect_equivalent(another_way, obs_stat_way)
})

test_that("conf_int argument works", {
  expect_equal(
    names(iris2 %>% 
      t_test(Sepal.Width ~ Species, order = c("virginica", "versicolor"),
             conf_int = FALSE)),
      c("statistic", "t_df", "p_value", "alternative")
  )
  expect_equal(
    names(iris2 %>% 
            t_test(Sepal.Width ~ Species, order = c("virginica", "versicolor"),
                   conf_int = TRUE)),
    c("statistic", "t_df", "p_value", "alternative", "lower_ci", "upper_ci")
  )
  
  ci_test <- iris2 %>% 
    t_test(Sepal.Width ~ Species, order = c("versicolor", "virginica"),
           conf_int = TRUE, conf_level = 0.9)
  old_way <- t.test(formula = Sepal.Width ~ Species, 
                    data = iris2,
                    conf.level = 0.9)[["conf.int"]]
  expect_equal(ci_test$lower_ci[1], old_way[1])
  expect_equal(ci_test$upper_ci[1], old_way[2])
  
  expect_error(
    iris2 %>% 
      t_test(Petal.Width ~ Species, order = c("versicolor", "virginica"),
             conf_int = TRUE, conf_level = 1.1)
  )
  
  # Check that var.equal produces different results
  set.seed(2018)
  iris_small <- iris2 %>% sample_n(10)
  no_var_equal <- iris_small %>% 
    t_stat(Petal.Width ~ Species, order = c("versicolor", "virginica"))
  var_equal <- iris_small %>% 
    t_stat(Petal.Width ~ Species, order = c("versicolor", "virginica"),
           var.equal = TRUE)
  expect_false(
    no_var_equal == var_equal
  )
  
})
