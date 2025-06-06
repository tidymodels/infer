set.seed(2018)
test_df <- gss_calc[1:20, ]
test_df$stat <- sample(c(
  -5,
  -4,
  -4,
  -4,
  -1,
  -0.5,
  rep(0, 6),
  1,
  1,
  3.999,
  4,
  4,
  4.001,
  5,
  5
))

test_that("direction is appropriate", {
  expect_snapshot(
    error = TRUE,
    test_df |> get_p_value(obs_stat = 0.5, direction = "righ")
  )
})

test_that("get_p_value works", {
  expect_equal(
    get_p_value(test_df, 4, "right")[[1]][1],
    5 / 20,
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "left")[[1]][1],
    17 / 20,
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "both")[[1]][1],
    10 / 20,
    tolerance = eps
  )

  expect_equal(
    get_p_value(test_df, 0, "right")[[1]][1],
    14 / 20,
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 0, "left")[[1]][1],
    12 / 20,
    tolerance = eps
  )
  # This is also a check for not returning value more than 1
  expect_equal(get_p_value(test_df, 0, "both")[[1]][1], 1, tolerance = eps)

  expect_equal(
    get_p_value(test_df, -3.999, "right")[[1]][1],
    16 / 20,
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, -3.999, "left")[[1]][1],
    4 / 20,
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, -3.999, "both")[[1]][1],
    8 / 20,
    tolerance = eps
  )

  expect_equal(
    get_p_value(test_df, 4, "greater"),
    get_p_value(test_df, 4, "right"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "less"),
    get_p_value(test_df, 4, "left"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two_sided"),
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two-sided"),
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two sided"),
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
  expect_equal(
    get_p_value(test_df, 4, "two.sided"),
    get_p_value(test_df, 4, "both"),
    tolerance = eps
  )
})

test_that("theoretical p-value not supported error", {
  obs_F <- gss_tbl |>
    specify(hours ~ partyid) |>
    calculate(stat = "F")
  expect_snapshot(
    error = TRUE,
    gss_tbl |>
      specify(hours ~ partyid) |>
      hypothesize(null = "independence") |>
      calculate(stat = "F") |>
      get_p_value(obs_stat = obs_F, direction = "right")
  )
})

test_that("get_p_value warns in case of zero p-value", {
  expect_snapshot(
    res_ <- get_p_value(gss_calc, obs_stat = -10, direction = "left")
  )
})

test_that("get_p_value throws error in case of `NaN` stat", {
  gss_calc$stat[1] <- NaN
  expect_snapshot(error = TRUE, res_ <- get_p_value(gss_calc, 0, "both"))

  gss_calc$stat[2] <- NaN
  expect_snapshot(error = TRUE, res_ <- get_p_value(gss_calc, 0, "both"))

  # In the case that _all_ values are NaN, error should have different text
  gss_calc$stat <- NaN
  expect_snapshot(error = TRUE, res_ <- get_p_value(gss_calc, 0, "both"))
})

test_that("get_p_value can handle fitted objects", {
  set.seed(1)

  null_fits <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    hypothesize(null = "independence") |>
    generate(reps = 10, type = "permute") |>
    fit()

  obs_fit <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    fit()

  expect_equal(
    get_p_value(null_fits, obs_fit, "both"),
    structure(
      list(
        term = c("age", "collegedegree", "intercept"),
        p_value = c(0.6, 0.4, 0.6)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    ignore_attr = TRUE
  )

  # errors out when it ought to
  obs_fit_2 <- gss[1:50, ] |>
    specify(hours ~ age) |>
    fit()

  expect_snapshot(error = TRUE, get_p_value(null_fits, obs_fit_2, "both"))

  obs_fit_3 <- gss[1:50, ] |>
    specify(year ~ age + college) |>
    fit()

  expect_snapshot(error = TRUE, get_p_value(null_fits, obs_fit_3, "both"))

  set.seed(1)

  null_fits_4 <- gss[1:50, ] |>
    specify(hours ~ age) |>
    hypothesize(null = "independence") |>
    generate(reps = 10, type = "permute") |>
    fit()

  obs_fit_4 <- gss[1:50, ] |>
    specify(hours ~ age) |>
    fit()

  obs_fit_4

  expect_equal(
    get_p_value(null_fits_4, obs_fit_4, "both"),
    structure(
      list(
        term = c("age", "intercept"),
        p_value = c(0.6, 0.6)
      ),
      row.names = c(NA, -2L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    ignore_attr = TRUE
  )

  expect_equal(ncol(null_fits_4), ncol(obs_fit_4) + 1)
  expect_equal(nrow(null_fits_4), nrow(obs_fit_4) * 10)

  expect_equal(ncol(obs_fit_4), ncol(obs_fit))
  expect_equal(nrow(obs_fit_4), nrow(obs_fit) - 1)

  expect_true(is_fitted(obs_fit))
  expect_true(is_fitted(obs_fit_2))
  expect_true(is_fitted(obs_fit_3))
  expect_true(is_fitted(obs_fit_4))

  expect_true(is_fitted(null_fits))
  expect_true(is_fitted(null_fits_4))
})

test_that("get_p_value can handle bad args with fitted objects", {
  set.seed(1)

  null_fits <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    hypothesize(null = "independence") |>
    generate(reps = 10, type = "permute") |>
    fit()

  obs_fit <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    fit()

  expect_snapshot(error = TRUE, get_p_value(null_fits, "boop", "both"))

  expect_snapshot(
    error = TRUE,
    get_p_value(null_fits, obs_fit$estimate, "both")
  )

  expect_snapshot(error = TRUE, get_p_value(obs_fit, null_fits, "both"))
})

test_that("get_p_value errors informatively when args are switched", {
  # switch obs_stat and x
  obs_stat <- gss |>
    specify(response = hours) |>
    calculate(stat = "mean")

  set.seed(1)

  null_dist <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 41) |>
    generate(reps = 20, type = "bootstrap") |>
    calculate(stat = "mean")

  expect_snapshot(error = TRUE, get_p_value(obs_stat, null_dist, "both"))

  expect_silent(
    get_p_value(null_dist, obs_stat, "both")
  )
})

test_that("get_p_value can handle theoretical distributions", {
  get_p_value_ <- function(x, obs_stat, direction) {
    x <- get_p_value(x, obs_stat, direction)

    x$p_value
  }

  # f ------------------------------------------------------------
  # direction = "right" is the only valid one
  f_dist <-
    gss |>
    specify(age ~ partyid) |>
    hypothesize(null = "independence") |>
    assume(distribution = "F")

  f_obs <-
    gss |>
    specify(age ~ partyid) |>
    calculate(stat = "F")

  expect_equal(
    get_p_value_(f_dist, f_obs, direction = "right"),
    0.06005251,
    tolerance = 1e-3
  )

  old_way_f <- broom::tidy(aov(age ~ partyid, gss))

  expect_equal(
    get_p_value_(f_dist, f_obs, direction = "right"),
    old_way_f$p.value[[1]],
    tolerance = 1e-3
  )

  # t ------------------------------------------------------------
  t_dist <-
    gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    assume("t")

  t_obs <-
    gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    calculate(stat = "t")

  expect_equal(
    get_p_value_(t_dist, t_obs, direction = "both"),
    0.03755,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(t_dist, t_obs, direction = "left"),
    0.981,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(t_dist, t_obs, direction = "right"),
    1 - get_p_value_(t_dist, t_obs, direction = "left"),
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(t_dist, t_obs, direction = "both"),
    (1 - get_p_value_(t_dist, t_obs, direction = "left")) * 2,
    tolerance = 1e-3
  )

  old_way_both <- t_test(gss, hours ~ NULL, mu = 40, alternative = "two.sided")

  expect_equal(
    old_way_both$p_value,
    get_p_value_(t_dist, t_obs, direction = "both"),
    tolerance = 1e-3
  )

  old_way_left <- t_test(gss, hours ~ NULL, mu = 40, alternative = "less")

  expect_equal(
    old_way_left$p_value,
    get_p_value_(t_dist, t_obs, direction = "left")
  )

  old_way_right <- t_test(gss, hours ~ NULL, mu = 40, alternative = "greater")

  expect_equal(
    old_way_right$p_value,
    get_p_value_(t_dist, t_obs, direction = "right")
  )

  # chisq ------------------------------------------------------------
  # direction = "right" is the only valid one
  chisq_dist <-
    gss |>
    specify(college ~ finrela) |>
    hypothesize(null = "independence") |>
    assume(distribution = "Chisq")

  chisq_obs <-
    gss |>
    specify(college ~ finrela) |>
    calculate(stat = "Chisq")

  expect_equal(
    get_p_value_(chisq_dist, chisq_obs, direction = "right"),
    1.082094e-05,
    tolerance = 1e-3
  )

  expect_snapshot(
    old_way <- chisq_test(gss, college ~ finrela)
  )

  expect_equal(
    old_way$p_value,
    get_p_value_(chisq_dist, chisq_obs, direction = "right"),
    tolerance = 1e-3
  )

  # z ------------------------------------------------------------
  z_dist <-
    gss |>
    specify(response = sex, success = "female") |>
    hypothesize(null = "point", p = .5) |>
    assume("z")

  z_obs <-
    gss |>
    specify(response = sex, success = "female") |>
    hypothesize(null = "point", p = .5) |>
    calculate(stat = "z")

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "both"),
    0.24492,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "left"),
    0.12246,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "right"),
    1 - get_p_value_(z_dist, z_obs, direction = "left"),
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "both"),
    (1 - get_p_value_(z_dist, z_obs, direction = "right")) * 2,
    tolerance = 1e-3
  )

  old_way_z_both <- prop_test(
    gss,
    sex ~ NULL,
    success = "female",
    p = .5,
    alternative = "two.sided",
    z = TRUE
  )
  old_way_z_left <- prop_test(
    gss,
    sex ~ NULL,
    success = "female",
    p = .5,
    alternative = "less",
    z = TRUE
  )
  old_way_z_right <- prop_test(
    gss,
    sex ~ NULL,
    success = "female",
    p = .5,
    alternative = "greater",
    z = TRUE
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "both"),
    old_way_z_both$p_value,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "left"),
    old_way_z_left$p_value,
    tolerance = 1e-3
  )

  expect_equal(
    get_p_value_(z_dist, z_obs, direction = "right"),
    old_way_z_right$p_value,
    tolerance = 1e-3
  )
})


test_that("get_p_value warns with bad theoretical distributions", {
  t_dist_40 <-
    gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    assume("t")

  t_dist_30 <-
    gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 30) |>
    assume("t")

  t_obs <-
    gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    calculate(stat = "t")

  expect_silent(
    get_p_value(
      t_dist_40,
      t_obs,
      direction = "both"
    )
  )

  expect_snapshot(
    res_ <- get_p_value(
      t_dist_30,
      t_obs,
      direction = "both"
    )
  )
})
