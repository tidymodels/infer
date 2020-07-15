context("wrappers")

test_that("t_test works", {
  # Two Sample
  expect_warning(gss_tbl %>% t_test(hours ~ sex))

  expect_error(
    gss_tbl %>% t_test(response = "hours", explanatory = "sex")
  )
  
  new_way <- t_test(gss_tbl, 
                    hours ~ sex, 
                    order = c("male", "female"))
  new_way_alt <- t_test(gss_tbl, 
                    response = hours,
                    explanatory = sex,
                    order = c("male", "female"))
  old_way <- t.test(hours ~ sex, data = gss_tbl) %>%
    broom::glance() %>%
    dplyr::select(statistic, t_df = parameter, p_value = p.value, 
                  alternative, lower_ci = conf.low, upper_ci = conf.high)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
  
  # check that the order argument changes output
  new_way2 <- t_test(gss_tbl, 
                    hours ~ sex, 
                    order = c("female", "male"))  
  expect_equal(new_way[["lower_ci"]], -new_way2[["upper_ci"]])
  expect_equal(new_way[["statistic"]], -new_way2[["statistic"]])
  
  # One Sample
  new_way <- gss_tbl %>%
    t_test(hours ~ NULL, mu = 0)
  new_way_alt <- gss_tbl %>%
    t_test(response = hours, mu = 0)
  old_way <- t.test(x = gss_tbl$hours, mu = 0) %>%
    broom::glance() %>%
    dplyr::select(statistic, t_df = parameter, p_value = p.value, 
                  alternative, lower_ci = conf.low, upper_ci = conf.high)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
})

test_that("chisq_test works", {
  # maleependence
  expect_silent(gss_tbl %>% 
                  chisq_test(college ~ partyid))
  new_way <- gss_tbl %>% 
    chisq_test(college ~ partyid)
  new_way_alt <- gss_tbl %>% 
    chisq_test(response = college, explanatory = partyid)
  old_way <- chisq.test(x = table(gss_tbl$partyid, gss_tbl$college)) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)

  expect_equal(new_way, new_way_alt, tolerance = eps)
  expect_equal(new_way, old_way, tolerance = eps)
  
  # Goodness of Fit
  expect_silent(gss_tbl %>% 
                  chisq_test(response = partyid, p = c(.3, .4, .3)))
  new_way <- gss_tbl %>% 
    chisq_test(partyid ~ NULL, p = c(.3, .4, .3))
  new_way_alt <- gss_tbl %>% 
    chisq_test(response = partyid, p = c(.3, .4, .3))
  old_way <- chisq.test(x = table(gss_tbl$partyid), p = c(.3, .4, .3)) %>%
    broom::glance() %>%
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)
  
  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
  
  # check that function errors out when response is numeric
  expect_error(chisq_test(x = gss_tbl, response = age, explanatory = partyid))
  
  # check that function errors out when explanatory is numeric
  expect_error(chisq_test(x = gss_tbl, response = partyid, explanatory = age))

})

test_that("_stat functions work", {
  # Test of maleependence
  expect_silent(gss_tbl %>% chisq_stat(college ~ partyid))
  another_way <- gss_tbl %>%
    chisq_test(college ~ partyid) %>%
    dplyr::select(statistic)
  obs_stat_way <- gss_tbl %>% chisq_stat(college ~ partyid)
  one_more <- chisq.test(
    table(gss_tbl$partyid, gss_tbl$college)
  )$statistic

  expect_equivalent(dplyr::pull(another_way), obs_stat_way)
  expect_equivalent(one_more, obs_stat_way)

  # Goodness of Fit
 new_way <- gss_tbl %>%
   chisq_test(partyid ~ NULL) %>%
   dplyr::select(statistic)
 obs_stat_way <- gss_tbl %>%
   chisq_stat(partyid ~ NULL)
 obs_stat_way_alt <- gss_tbl %>%
   chisq_stat(response = partyid)
 
 expect_equivalent(dplyr::pull(new_way), obs_stat_way)
 expect_equivalent(dplyr::pull(new_way), obs_stat_way_alt)
 
 # robust to the named vector
 unordered_p <- gss_tbl %>%
   chisq_test(response = partyid, p = c(.2, .3, .5))
 ordered_p <- gss_tbl %>%
   chisq_test(response = partyid, p = c(ind = .2, rep = .3, dem = .5))
 
 expect_equivalent(unordered_p, ordered_p)

  # Two sample t
  expect_silent(
    gss_tbl %>% t_stat(
      hours ~ sex, order = c("male", "female")
    )
  )
  another_way <- gss_tbl %>%
    t_test(hours ~ sex, order = c("male", "female")) %>%
    dplyr::select(statistic) %>%
    pull()
  obs_stat_way <- gss_tbl %>%
    t_stat(hours ~ sex, order = c("male", "female"))
  obs_stat_way_alt <- gss_tbl %>%
    t_stat(response = hours,
           explanatory = sex, 
           order = c("male", "female"))
  
  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(another_way, obs_stat_way_alt)

  # One sample t
  expect_silent(gss_tbl %>% t_stat(hours ~ NULL))
  another_way <- gss_tbl %>%
    t_test(hours ~ NULL) %>%
    dplyr::select(statistic) %>%
    pull()
  obs_stat_way <- gss_tbl %>%
    t_stat(hours ~ NULL)
  obs_stat_way_alt <- gss_tbl %>%
    t_stat(response = hours)
  
  expect_equivalent(another_way, obs_stat_way)
  expect_equivalent(another_way, obs_stat_way_alt)
  
  expect_error(chisq_stat(x = gss_tbl, response = age, explanatory = sex))
  expect_error(chisq_stat(x = gss_tbl, response = sex, explanatory = age))
})

test_that("conf_int argument works", {
  expect_equal(
    names(
      gss_tbl %>% 
        t_test(hours ~ sex, 
               order = c("male", "female"), conf_int = FALSE)
    ),
    c("statistic", "t_df", "p_value", "alternative"), 
    tolerance = 1e-5
  )
  expect_equal(
    names(
      gss_tbl %>%
        t_test(
          hours ~ sex, order = c("male", "female"),
          conf_int = TRUE
        )
    ),
    c("statistic", "t_df", "p_value", "alternative", "lower_ci", "upper_ci"),
    tolerance = 1e-5
  )

  ci_test <- gss_tbl %>%
    t_test(
      hours ~ sex, order = c("male", "female"),
      conf_int = TRUE, conf_level = 0.9
    )
  old_way <- t.test(
    formula = hours ~ sex, data = gss_tbl, conf.level = 0.9
  )[["conf.int"]]
  expect_equal(ci_test$lower_ci[1], old_way[1], tolerance = 1e-5)
  expect_equal(ci_test$upper_ci[1], old_way[2], tolerance = 1e-5)

  expect_error(
    gss_tbl %>%
      t_test(
        hours ~ sex, order = c("female", "male"),
        conf_int = TRUE, conf_level = 1.1
      )
  )

  # Check that var.equal produces different results
  # Thanks for fmaleing this @EllaKaye!
  gss_tbl_small <- gss_tbl %>% slice(1:6, 90:100)

  no_var_equal <- gss_tbl_small %>%
    t_stat(hours ~ sex, order = c("female", "male"))
  
  var_equal <- gss_tbl_small %>%
    t_stat(
      hours ~ sex, order = c("female", "male"),
      var.equal = TRUE
    ) 
  expect_false(no_var_equal == var_equal)

  shortcut_no_var_equal <- gss_tbl_small %>%
    specify(hours ~ sex) %>%
    calculate(stat = "t", order = c("female", "male"))

  shortcut_var_equal <- gss_tbl_small %>%
    specify(hours ~ sex) %>%
    calculate(
      stat = "t", order = c("female", "male"),
      var.equal = TRUE
    )
  expect_false(shortcut_no_var_equal == shortcut_var_equal)
})

# generate some data to test the prop.test wrapper
df <- data.frame(resp = c(rep("c", 450), 
                          rep("d", 50), 
                          rep("c", 400), 
                          rep("d", 100)), 
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
