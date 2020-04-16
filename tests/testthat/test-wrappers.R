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
  expect_warning(iris2 %>% t_test(Sepal.Width ~ Species))

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

  expect_equal(new_way, new_way_alt, tolerance = .Machine$double.eps^0.25)
  #temporary remove because of failing noLD
  #expect_equal(new_way, old_way, tolerance = .Machine$double.eps^0.25)
  
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
  
  # check that function errors out when response is numeric
  expect_error(chisq_test(x = iris2, response = Sepal.Length, explanatory = Species))
  
  # check that function errors out when explanatory is numeric
  expect_error(chisq_test(x = iris2, response = Species, explanatory = Sepal.Length))

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

  expect_equivalent(dplyr::pull(another_way), obs_stat_way)
  expect_equivalent(one_more, obs_stat_way)

  # Goodness of Fit
 new_way <- iris3 %>%
   chisq_test(Species ~ NULL) %>%
   dplyr::select(statistic)
 obs_stat_way <- iris3 %>%
   chisq_stat(Species ~ NULL)
 obs_stat_way_alt <- iris3 %>%
   chisq_stat(response = Species)
 
 expect_equivalent(dplyr::pull(new_way), obs_stat_way)
 expect_equivalent(dplyr::pull(new_way), obs_stat_way_alt)
 
 # robust to the named vector
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
    dplyr::select(statistic) %>%
    pull()
  obs_stat_way <- iris2 %>%
    t_stat(Sepal.Width ~ Species, order = c("virginica", "versicolor"))
  obs_stat_way_alt <- iris2 %>%
    t_stat(response = Sepal.Width,
           explanatory = Species, 
           order = c("virginica", "versicolor"))
  
  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(another_way, obs_stat_way_alt)

  # One sample t
  expect_silent(iris2 %>% t_stat(Sepal.Width ~ NULL))
  another_way <- iris2 %>%
    t_test(Sepal.Width ~ NULL) %>%
    dplyr::select(statistic) %>%
    pull()
  obs_stat_way <- iris2 %>%
    t_stat(Sepal.Width ~ NULL)
  obs_stat_way_alt <- iris2 %>%
    t_stat(response = Sepal.Width)
  
  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(another_way, obs_stat_way_alt)
  
  expect_error(chisq_stat(x = iris2, response = Sepal.Length, explanatory = Species))
  expect_error(chisq_stat(x = iris2, response = Species, explanatory = Sepal.Length))
})

test_that("conf_int argument works", {
  expect_equal(
    names(
      iris2 %>% 
        t_test(Sepal.Width ~ Species, 
               order = c("virginica", "versicolor"), conf_int = FALSE)
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
  iris_small <- iris2 %>% slice(1:6, 90:100)

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

# generate some data to test the prop.test wrapper
df <- data.frame(resp = c(rep("c", 450), rep("d", 50), rep("c", 400), rep("d", 100)), 
                 exp = rep(c("a", "b"), each = 500))

sum_df <- table(df)

bad_df <- data.frame(resp = 1:5, 
                     exp = letters[1:5])

bad_df2 <- data.frame(resp = letters[1:5], 
                     exp = 1:5)

test_that("two sample prop_test works", {

  # run the tests with default args
  base <- prop.test(sum_df)
  infer <- prop_test(df, resp ~ exp, order = c("a", "b"))
  
  # check that results are same
  expect_equal(base[["statistic"]], 
               infer[["statistic"]], 
               tolerance = .001)
  expect_equal(base[["parameter"]], 
               infer[["chisq_df"]])
  expect_equal(base[["p.value"]], 
               infer[["p_value"]],
               tolerance = .001)

  # expect warning for unspecified order
  expect_warning(prop_test(df, resp ~ exp))
  
  # check that the functions respond to "p" in the same way
  base2 <- prop.test(sum_df, p = c(.1, .1))
  infer2 <- prop_test(df, resp ~ exp, order = c("a", "b"), p = c(.1, .1))
  expect_equal(base2[["statistic"]], 
               infer2[["statistic"]], 
               tolerance = .001)
  expect_equal(base2[["parameter"]], 
               infer2[["chisq_df"]])
  expect_equal(base2[["p.value"]], 
               infer2[["p_value"]],
               tolerance = .001)
  
  # check confidence interval argument
  infer3 <- prop_test(df, resp ~ exp, order = c("a", "b"), conf_int = TRUE)
  expect_length(infer3, 6)
  expect_length(infer2, 4)
  
  # check that the order argument changes output
  infer4 <- prop_test(df, resp ~ exp, order = c("b", "a"), conf_int = TRUE)
  expect_equal(infer4[["lower_ci"]], -infer3[["upper_ci"]])
  
  expect_error(prop_test(bad_df, resp ~ exp))
  expect_error(prop_test(bad_df2, resp ~ exp))
})

# ...and some data for the one sample wrapper
df_1 <- df %>%
  select(resp)

sum_df_1 <- table(df_1)

test_that("one sample prop_test works", {
  
  # check that results with default args are the same
  base <- prop.test(sum_df_1)
  infer <- prop_test(df_1, resp ~ NULL, p = .5)
  expect_equal(base[["statistic"]], 
               infer[["statistic"]], 
               tolerance = .001)
  expect_equal(base[["parameter"]], 
               infer[["chisq_df"]])
  expect_equal(base[["p.value"]], 
               infer[["p_value"]],
               tolerance = .001)
  
  # check that the functions respond to "p" in the same way
  base2 <- prop.test(sum_df_1, p = .86)
  infer2 <- prop_test(df_1, resp ~ NULL, p = .86)
  expect_equal(base2[["statistic"]], 
               infer2[["statistic"]], 
               tolerance = .001)
  expect_equal(base2[["parameter"]], 
               infer2[["chisq_df"]])
  expect_equal(base2[["p.value"]], 
               infer2[["p_value"]],
               tolerance = .001)
  
  # expect message for unspecified p
  expect_message(prop_test(df_1, resp ~ NULL))
})
