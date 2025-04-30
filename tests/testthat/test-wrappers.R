test_that("t_test works", {
  # Two Sample
  expect_snapshot(res_ <- gss_tbl |> t_test(hours ~ sex))

  expect_snapshot(
    error = TRUE,
    gss_tbl |> t_test(response = "hours", explanatory = "sex")
  )

  new_way <- t_test(gss_tbl, hours ~ sex, order = c("male", "female"))
  new_way_alt <- t_test(
    gss_tbl,
    response = hours,
    explanatory = sex,
    order = c("male", "female")
  )
  old_way <- t.test(hours ~ sex, data = gss_tbl) |>
    broom::glance() |>
    dplyr::select(
      statistic,
      t_df = parameter,
      p_value = p.value,
      alternative,
      estimate,
      lower_ci = conf.low,
      upper_ci = conf.high
    )

  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)

  # check that the order argument changes output
  new_way2 <- t_test(gss_tbl, hours ~ sex, order = c("female", "male"))
  expect_equal(new_way[["lower_ci"]], -new_way2[["upper_ci"]])
  expect_equal(new_way[["statistic"]], -new_way2[["statistic"]])

  # One Sample
  new_way <- gss_tbl |>
    t_test(hours ~ NULL, mu = 0)
  new_way_alt <- gss_tbl |>
    t_test(response = hours, mu = 0)
  old_way <- t.test(x = gss_tbl$hours, mu = 0) |>
    broom::glance() |>
    dplyr::select(
      statistic,
      t_df = parameter,
      p_value = p.value,
      alternative,
      estimate,
      lower_ci = conf.low,
      upper_ci = conf.high
    )

  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)
})

test_that("chisq_test works", {
  # maleependence
  expect_silent(
    gss_tbl |>
      chisq_test(college ~ partyid)
  )
  new_way <- gss_tbl |>
    chisq_test(college ~ partyid)
  new_way_alt <- gss_tbl |>
    chisq_test(response = college, explanatory = partyid)
  old_way <- chisq.test(x = table(gss_tbl$partyid, gss_tbl$college)) |>
    broom::glance() |>
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)

  expect_equal(new_way, new_way_alt, tolerance = eps)
  expect_equal(new_way, old_way, tolerance = eps)

  # Goodness of Fit
  expect_silent(
    gss_tbl |>
      chisq_test(response = partyid, p = c(.3, .4, .3))
  )
  new_way <- gss_tbl |>
    chisq_test(partyid ~ NULL, p = c(.3, .4, .3))
  new_way_alt <- gss_tbl |>
    chisq_test(response = partyid, p = c(.3, .4, .3))
  old_way <- chisq.test(x = table(gss_tbl$partyid), p = c(.3, .4, .3)) |>
    broom::glance() |>
    dplyr::select(statistic, chisq_df = parameter, p_value = p.value)

  expect_equal(new_way, new_way_alt, tolerance = 1e-5)
  expect_equal(new_way, old_way, tolerance = 1e-5)

  # check that function errors out when response is numeric
  expect_snapshot(
    error = TRUE,
    chisq_test(x = gss_tbl, response = age, explanatory = partyid)
  )

  # check that function errors out when explanatory is numeric
  expect_snapshot(
    error = TRUE,
    chisq_test(x = gss_tbl, response = partyid, explanatory = age)
  )
})

test_that("_stat functions work", {
  # Test of maleependence
  expect_snapshot(
    res_ <- gss_tbl |> chisq_stat(college ~ partyid)
  )

  another_way <- gss_tbl |>
    chisq_test(college ~ partyid) |>
    dplyr::select(statistic)

  expect_snapshot(
    obs_stat_way <- gss_tbl |> chisq_stat(college ~ partyid)
  )
  one_more <- chisq.test(
    table(gss_tbl$partyid, gss_tbl$college)
  )$statistic

  expect_equal(dplyr::pull(another_way), obs_stat_way, ignore_attr = TRUE)
  expect_equal(one_more, obs_stat_way, ignore_attr = TRUE)

  # Goodness of Fit
  new_way <- gss_tbl |>
    chisq_test(partyid ~ NULL) |>
    dplyr::select(statistic)

  expect_snapshot(
    obs_stat_way <- gss_tbl |>
      chisq_stat(partyid ~ NULL)
  )
  expect_snapshot(
    obs_stat_way_alt <- gss_tbl |>
      chisq_stat(response = partyid)
  )

  expect_equal(dplyr::pull(new_way), obs_stat_way, ignore_attr = TRUE)
  expect_equal(dplyr::pull(new_way), obs_stat_way_alt, ignore_attr = TRUE)

  # robust to the named vector
  unordered_p <- gss_tbl |>
    chisq_test(response = partyid, p = c(.2, .3, .5))
  ordered_p <- gss_tbl |>
    chisq_test(response = partyid, p = c(ind = .2, rep = .3, dem = .5))

  expect_equal(unordered_p, ordered_p, ignore_attr = TRUE)

  # Two sample t
  expect_snapshot(
    res_ <- gss_tbl |>
      t_stat(
        hours ~ sex,
        order = c("male", "female")
      )
  )
  another_way <- gss_tbl |>
    t_test(hours ~ sex, order = c("male", "female")) |>
    dplyr::select(statistic) |>
    pull()

  expect_snapshot(
    obs_stat_way <- gss_tbl |>
      t_stat(hours ~ sex, order = c("male", "female"))
  )

  expect_snapshot(
    obs_stat_way_alt <- gss_tbl |>
      t_stat(response = hours, explanatory = sex, order = c("male", "female"))
  )

  expect_equal(another_way, obs_stat_way, ignore_attr = TRUE)
  expect_equal(another_way, obs_stat_way_alt, ignore_attr = TRUE)

  # One sample t
  expect_snapshot(
    res_ <- gss_tbl |> t_stat(hours ~ NULL)
  )

  another_way <- gss_tbl |>
    t_test(hours ~ NULL) |>
    dplyr::select(statistic) |>
    pull()

  expect_snapshot(
    obs_stat_way <- gss_tbl |>
      t_stat(hours ~ NULL)
  )
  expect_snapshot(
    obs_stat_way_alt <- gss_tbl |>
      t_stat(response = hours)
  )

  expect_equal(another_way, obs_stat_way, ignore_attr = TRUE)
  expect_equal(another_way, obs_stat_way_alt, ignore_attr = TRUE)

  expect_snapshot(
    error = TRUE,
    res_ <- chisq_stat(x = gss_tbl, response = age, explanatory = sex)
  )

  expect_snapshot(
    error = TRUE,
    res_ <- chisq_stat(x = gss_tbl, response = sex, explanatory = age)
  )
})

test_that("conf_int argument works", {
  expect_equal(
    names(
      gss_tbl |>
        t_test(hours ~ sex, order = c("male", "female"), conf_int = FALSE)
    ),
    c("statistic", "t_df", "p_value", "alternative", "estimate"),
    tolerance = 1e-5
  )
  expect_equal(
    names(
      gss_tbl |>
        t_test(
          hours ~ sex,
          order = c("male", "female"),
          conf_int = TRUE
        )
    ),
    c(
      "statistic",
      "t_df",
      "p_value",
      "alternative",
      "estimate",
      "lower_ci",
      "upper_ci"
    ),
    tolerance = 1e-5
  )

  ci_test <- gss_tbl |>
    t_test(
      hours ~ sex,
      order = c("male", "female"),
      conf_int = TRUE,
      conf_level = 0.9
    )
  old_way <- t.test(
    formula = hours ~ sex,
    data = gss_tbl,
    conf.level = 0.9
  )[["conf.int"]]
  expect_equal(ci_test$lower_ci[1], old_way[1], tolerance = 1e-5)
  expect_equal(ci_test$upper_ci[1], old_way[2], tolerance = 1e-5)

  expect_snapshot(
    error = TRUE,
    res_ <- gss_tbl |>
      t_test(
        hours ~ sex,
        order = c("female", "male"),
        conf_int = TRUE,
        conf_level = 1.1
      )
  )

  # Check that var.equal produces different results
  # Thanks for fmaleing this @EllaKaye!
  gss_tbl_small <- gss_tbl |> dplyr::slice(1:6, 90:100)

  expect_snapshot(
    no_var_equal <- gss_tbl_small |>
      t_stat(hours ~ sex, order = c("female", "male"))
  )

  expect_snapshot(
    var_equal <- gss_tbl_small |>
      t_stat(
        hours ~ sex,
        order = c("female", "male"),
        var.equal = TRUE
      )
  )

  expect_false(no_var_equal == var_equal)

  shortcut_no_var_equal <- gss_tbl_small |>
    specify(hours ~ sex) |>
    calculate(stat = "t", order = c("female", "male"))

  shortcut_var_equal <- gss_tbl_small |>
    specify(hours ~ sex) |>
    calculate(
      stat = "t",
      order = c("female", "male"),
      var.equal = TRUE
    )
  expect_false(shortcut_no_var_equal == shortcut_var_equal)
})

# generate some data to test the prop.test wrapper
df <- data.frame(
  exp = rep(c("a", "b"), each = 500),
  resp = c(rep("c", 450), rep("d", 50), rep("c", 400), rep("d", 100)),
  stringsAsFactors = FALSE
)

sum_df <- table(df)

bad_df <- data.frame(resp = 1:5, exp = letters[1:5])

bad_df2 <- data.frame(resp = letters[1:5], exp = 1:5)

df_l <- df |>
  dplyr::mutate(resp = dplyr::if_else(resp == "c", TRUE, FALSE))

test_that("two sample prop_test works", {
  # run the tests with default args
  base <- prop.test(sum_df)
  infer <- prop_test(df, resp ~ exp, order = c("a", "b"))

  # check that results are same
  expect_equal(base[["statistic"]], infer[["statistic"]], tolerance = .001)
  expect_equal(base[["parameter"]], infer[["chisq_df"]])
  expect_equal(base[["p.value"]], infer[["p_value"]], tolerance = .001)

  # expect warning for unspecified order
  expect_snapshot(res_ <- prop_test(df, resp ~ exp))

  # check that the functions respond to "p" in the same way
  base2 <- prop.test(sum_df, p = c(.1, .1))
  infer2 <- prop_test(df, resp ~ exp, order = c("a", "b"), p = c(.1, .1))
  expect_equal(base2[["statistic"]], infer2[["statistic"]], tolerance = .001)
  expect_equal(base2[["parameter"]], infer2[["chisq_df"]])
  expect_equal(base2[["p.value"]], infer2[["p_value"]], tolerance = .001)

  # check confidence interval argument
  infer3 <- prop_test(df, resp ~ exp, order = c("a", "b"), conf_int = TRUE)
  expect_length(infer3, 6)
  expect_length(infer2, 4)

  # check that the order argument changes output
  infer4 <- prop_test(df, resp ~ exp, order = c("b", "a"), conf_int = TRUE)
  expect_equal(infer4[["lower_ci"]], -infer3[["upper_ci"]], tolerance = .001)

  expect_snapshot(error = TRUE, res_ <- prop_test(bad_df, resp ~ exp))
  expect_snapshot(error = TRUE, res_ <- prop_test(bad_df2, resp ~ exp))

  # check that the success argument changes output
  infer5 <- prop_test(
    df,
    resp ~ exp,
    order = c("a", "b"),
    success = "d",
    conf_int = TRUE
  )
  expect_equal(infer3[["upper_ci"]], -infer5[["lower_ci"]], tolerance = .001)

  # check that logical variables are leveled intuitively
  infer1_l <- prop_test(df_l, resp ~ exp, order = c("b", "a"))
  infer2_l <- prop_test(df_l, resp ~ exp, order = c("b", "a"), success = "TRUE")
  infer3_l <- prop_test(
    df_l,
    resp ~ exp,
    order = c("b", "a"),
    success = "FALSE"
  )

  expect_equal(infer1_l$lower_ci, infer2_l$lower_ci)
  expect_equal(infer1_l$lower_ci, -infer3_l$upper_ci)
})

# ...and some data for the one sample wrapper
df_1 <- df |>
  select(resp)

sum_df_1 <- table(df_1)

test_that("one sample prop_test works", {
  # check that results with default args are the same
  base <- prop.test(sum_df_1)
  infer <- prop_test(df_1, resp ~ NULL, p = .5)
  expect_equal(base[["statistic"]], infer[["statistic"]], tolerance = .001)
  expect_equal(base[["parameter"]], infer[["chisq_df"]])
  expect_equal(base[["p.value"]], infer[["p_value"]], tolerance = .001)

  # check that the functions respond to "p" in the same way
  base2 <- prop.test(sum_df_1, p = .86)
  infer2 <- prop_test(df_1, resp ~ NULL, p = .86)
  expect_equal(base2[["statistic"]], infer2[["statistic"]], tolerance = .001)
  expect_equal(base2[["parameter"]], infer2[["chisq_df"]])
  expect_equal(base2[["p.value"]], infer2[["p_value"]], tolerance = .001)

  # expect message for unspecified p
  expect_snapshot(res_ <- prop_test(df_1, resp ~ NULL))

  # check that the success argument changes output
  infer3 <- prop_test(df_1, resp ~ NULL, p = .2, success = "c")
  infer4 <- prop_test(df_1, resp ~ NULL, p = .8, success = "d")
  expect_equal(infer3[["chisq_df"]], infer4[["chisq_df"]], tolerance = .001)
  expect_snapshot(
    error = TRUE,
    res_ <- prop_test(df_1, resp ~ NULL, p = .2, success = "b")
  )
})

test_that("prop_test output dimensionality is correct", {
  infer_1_sample <- prop_test(df, resp ~ NULL, p = .5)
  infer_1_sample_z <- prop_test(df, resp ~ NULL, p = .5, z = TRUE)
  infer_2_sample <- prop_test(df, resp ~ exp, order = c("a", "b"))
  infer_2_sample_no_int <- prop_test(
    df,
    resp ~ exp,
    order = c("a", "b"),
    conf_int = FALSE
  )
  infer_2_sample_z <- prop_test(df, resp ~ exp, order = c("a", "b"), z = TRUE)

  expect_length(infer_1_sample, 4)
  expect_length(infer_1_sample, length(infer_1_sample_z) + 1)
  expect_length(infer_2_sample, 6)
  expect_length(infer_2_sample_no_int, 4)
  expect_length(infer_2_sample_z, length(infer_2_sample) - 1)
})

test_that("prop_test handles >2 explanatory levels gracefully", {
  set.seed(1)
  dfr <-
    tibble::tibble(
      exp = sample(c("a", "b", "c"), 100, replace = TRUE),
      resp = sample(c("d", "e"), 100, replace = TRUE)
    )

  res_old <- prop.test(table(dfr))

  # don't pass order
  expect_silent(
    res_1 <- prop_test(dfr, resp ~ exp)
  )

  # pass 2-length order
  expect_snapshot(
    res_2 <- prop_test(dfr, resp ~ exp, order = c("a", "b"))
  )

  # pass 3-length order
  expect_snapshot(
    res_3 <- prop_test(dfr, resp ~ exp, order = c("a", "b", "c"))
  )

  expect_equal(res_1, res_2)
  expect_equal(res_2, res_3)

  expect_named(res_1, c("statistic", "chisq_df", "p_value"))
  expect_equal(res_1$statistic, res_old$statistic)
  expect_equal(res_1$chisq_df, res_old$parameter)
  expect_equal(res_1$p_value, res_old$p.value)
})

test_that("prop_test errors with >2 response levels", {
  set.seed(1)
  dfr <-
    tibble::tibble(
      exp = sample(c("a", "b"), 100, replace = TRUE),
      resp = sample(c("c", "d", "e"), 100, replace = TRUE)
    )

  expect_snapshot(
    error = TRUE,
    res_1 <- prop_test(dfr, resp ~ exp)
  )
})

test_that("prop_test z argument works as expected", {
  chi_res <- prop_test(df, resp ~ NULL, p = .5, correct = FALSE)

  z_res <- prop_test(df, resp ~ NULL, p = .5, z = TRUE)

  expect_equal(unname(chi_res$statistic), z_res$statistic^2, tolerance = eps)
})

test_that("wrappers can handled ordered factors", {
  expect_equal(
    gss_tbl |>
      dplyr::mutate(sex = factor(sex, ordered = FALSE)) |>
      t_test(hours ~ sex, order = c("male", "female")),
    gss_tbl |>
      dplyr::mutate(sex = factor(sex, ordered = TRUE)) |>
      t_test(hours ~ sex, order = c("male", "female"))
  )

  expect_snapshot(
    ordered_t_1 <- gss_tbl |>
      dplyr::mutate(income = factor(income, ordered = TRUE)) |>
      chisq_test(income ~ partyid)
  )

  expect_snapshot(
    ordered_f_1 <- gss_tbl |>
      dplyr::mutate(income = factor(income, ordered = FALSE)) |>
      chisq_test(income ~ partyid)
  )

  expect_equal(ordered_t_1, ordered_f_1)

  expect_snapshot(
    ordered_t_2 <- gss_tbl |>
      dplyr::mutate(income = factor(income, ordered = TRUE)) |>
      chisq_test(partyid ~ income)
  )

  expect_snapshot(
    ordered_f_2 <- gss_tbl |>
      dplyr::mutate(income = factor(income, ordered = FALSE)) |>
      chisq_test(partyid ~ income)
  )

  expect_equal(ordered_t_2, ordered_f_2)

  expect_equal(
    df |>
      dplyr::mutate(resp = factor(resp, ordered = TRUE)) |>
      prop_test(resp ~ NULL, p = .5),
    df |>
      dplyr::mutate(resp = factor(resp, ordered = FALSE)) |>
      prop_test(resp ~ NULL, p = .5)
  )
})

test_that("handles spaces in variable names (t_test)", {
  gss_ <- gss |>
    tidyr::drop_na(college) |>
    dplyr::mutate(`h o u r s` = hours)

  expect_equal(
    t_test(
      gss_,
      formula = hours ~ college,
      order = c("degree", "no degree"),
      alternative = "two-sided"
    ),
    t_test(
      gss_,
      formula = `h o u r s` ~ college,
      order = c("degree", "no degree"),
      alternative = "two-sided"
    )
  )

  expect_equal(
    t_test(
      gss_,
      response = hours,
      explanatory = college,
      order = c("degree", "no degree"),
      alternative = "two-sided"
    ),
    t_test(
      gss_,
      response = `h o u r s`,
      explanatory = college,
      order = c("degree", "no degree"),
      alternative = "two-sided"
    )
  )
})

test_that("handles spaces in variable names (prop_test)", {
  df$`r e s p` <- df$resp

  expect_equal(
    prop_test(df, `r e s p` ~ exp, order = c("a", "b")),
    prop_test(df, resp ~ exp, order = c("a", "b"))
  )

  expect_equal(
    prop_test(df, response = `r e s p`, explanatory = exp, order = c("a", "b")),
    prop_test(df, response = resp, explanatory = exp, order = c("a", "b"))
  )
})
