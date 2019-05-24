context("wrappers")

iris2 <- iris %>%
  dplyr::filter(Species != "setosa") %>%
  droplevels(.$Species)

iris3 <- iris %>%
  dplyr::mutate(
    Sepal.Length.Group = dplyr::if_else(Sepal.Length > 5, ">5", "<=5")
  )

test_that("t_test works", {
  # Two Sample
  expect_error(iris2 %>% t_test(Sepal.Width ~ Species))

  expect_error(
    iris2 %>% t_test(response = "Sepal.Width", explanatory = "Species")
  )
  
  new_way <- t_test(iris2, 
                    Sepal.Width ~ Species, 
                    order = c("versicolor", "virginica"))
  new_way_alt <- t_test(iris2, 
                    response = Sepal.Width,
                    explanatory = Species,
                    order = c("versicolor", "virginica"))
  old_way <- t.test(Sepal.Width ~ Species, data = iris2) %>%
    broom::glance() %>%
    dplyr::select(statistic, t_df = parameter, p_value = p.value, 
                  alternative, lower_ci = conf.low, upper_ci = conf.high)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
  
  # One Sample
  new_way <- iris2 %>%
    t_test(Sepal.Width ~ NULL, mu = 0)
  new_way_alt <- iris2 %>%
    t_test(response = Sepal.Width, mu = 0)
  old_way <- t.test(x = iris2$Sepal.Width, mu = 0) %>%
    broom::glance() %>%
    dplyr::select(statistic, t_df = parameter, p_value = p.value, 
                  alternative, lower_ci = conf.low, upper_ci = conf.high)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
})

test_that("chisq_test works", {
  # Independence
  expect_silent(iris3 %>% 
                  chisq_test(Sepal.Length.Group ~ Species))
  new_way <- iris3 %>% 
    chisq_test(Sepal.Length.Group ~ Species)
  new_way_alt <- iris3 %>% 
    chisq_test(response = Sepal.Length.Group, explanatory = Species)
  old_way <- chisq.test(x = table(iris3$Species, iris3$Sepal.Length.Group)) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)

  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
  
  # Goodness of Fit
  expect_silent(iris3 %>% 
                  chisq_test(response = Species, p = c(.3, .4, .3)))
  new_way <- iris3 %>% 
    chisq_test(Species ~ NULL, p = c(.3, .4, .3))
  new_way_alt <- iris3 %>% 
    chisq_test(response = Species, p = c(.3, .4, .3))
  old_way <- chisq.test(x = table(iris3$Species), p = c(.3, .4, .3)) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
})

test_that("_stat functions work", {
  # Test of independence
  expect_silent(iris3 %>% chisq_stat(Sepal.Length.Group ~ Species))
  another_way <- iris3 %>%
    chisq_test(Sepal.Length.Group ~ Species) %>%
    dplyr::select(statistic)
  obs_stat_way <- iris3 %>% chisq_stat(Sepal.Length.Group ~ Species)
  one_more <- chisq.test(
    table(iris3$Species, iris3$Sepal.Length.Group)
  )$statistic

  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(one_more, dplyr::pull(obs_stat_way))

  # Goodness of Fit
 new_way <- iris3 %>%
   chisq_test(Species ~ NULL) %>%
   dplyr::select(statistic)
 obs_stat_way <- iris3 %>%
   chisq_stat(Species ~ NULL)
 obs_stat_way_alt <- iris3 %>%
   chisq_stat(response = Species)
 
 expect_equivalent(another_way, obs_stat_way)
 expect_equivalent(another_way, obs_stat_way_alt)
 
 unordered_p <- iris3 %>%
   chisq_test(response = Species, p = c(.2, .3, .5))
 ordered_p <- iris3 %>%
   chisq_test(response = Species, p = c(virginica = .5, versicolor = .3, setosa = .2))
 
 expect_equivalent(unordered_p, ordered_p)

  # Two sample t
  expect_silent(
    iris2 %>% t_stat(
      Sepal.Width ~ Species, order = c("virginica", "versicolor")
    )
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
    names(
      iris2 %>%
        t_test(
          Sepal.Width ~ Species, order = c("virginica", "versicolor"),
          conf_int = FALSE
        )
    ),
    c("statistic", "t_df", "p_value", "alternative"), 
    tolerance = 1e-5
  )
  expect_equal(
    names(
      iris2 %>%
        t_test(
          Sepal.Width ~ Species, order = c("virginica", "versicolor"),
          conf_int = TRUE
        )
    ),
    c("statistic", "t_df", "p_value", "alternative", "lower_ci", "upper_ci"),
    tolerance = 1e-5
  )

  ci_test <- iris2 %>%
    t_test(
      Sepal.Width ~ Species, order = c("versicolor", "virginica"),
      conf_int = TRUE, conf_level = 0.9
    )
  old_way <- t.test(
    formula = Sepal.Width ~ Species, data = iris2, conf.level = 0.9
  )[["conf.int"]]
  expect_equal(ci_test$lower_ci[1], old_way[1], tolerance = 1e-5)
  expect_equal(ci_test$upper_ci[1], old_way[2], tolerance = 1e-5)

  expect_error(
    iris2 %>%
      t_test(
        Petal.Width ~ Species, order = c("versicolor", "virginica"),
        conf_int = TRUE, conf_level = 1.1
      )
  )

  # Check that var.equal produces different results
  # Thanks for finding this @EllaKaye!
  set.seed(2018)
  iris_small <- iris2 %>% sample_n(10)
  no_var_equal <- iris_small %>%
    t_stat(Petal.Width ~ Species, order = c("versicolor", "virginica"))
  var_equal <- iris_small %>%
    t_stat(
      Petal.Width ~ Species, order = c("versicolor", "virginica"),
      var.equal = TRUE
    )
  expect_false(no_var_equal == var_equal)

  shortcut_no_var_equal <- iris_small %>%
    specify(Petal.Width ~ Species) %>%
    calculate(stat = "t", order = c("versicolor", "virginica"))

  shortcut_var_equal <- iris_small %>%
    specify(Petal.Width ~ Species) %>%
    calculate(
      stat = "t", order = c("versicolor", "virginica"),
      var.equal = TRUE
    )
  expect_false(shortcut_no_var_equal == shortcut_var_equal)
})
