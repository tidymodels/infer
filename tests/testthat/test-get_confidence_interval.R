set.seed(2018)
test_df <- gss_calc[1:20, ]
test_df$stat <- c(
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
)
point <- mean(test_df[["stat"]])

perc_def_out <- tibble::tibble(
  lower_ci = unname(quantile(test_df[["stat"]], 0.025)),
  upper_ci = unname(quantile(test_df[["stat"]], 0.975))
)

test_that("get_confidence_interval works with defaults", {
  expect_equal(test_df |> get_confidence_interval(), perc_def_out)
})

test_that("get_confidence_interval works with `type = 'percentile'`", {
  expect_equal(
    test_df |> get_confidence_interval(type = "percentile"),
    perc_def_out
  )

  expect_equal(
    test_df |> get_confidence_interval(level = 0.5, type = "percentile"),
    tibble::tibble(
      lower_ci = unname(quantile(test_df[["stat"]], 0.25)),
      upper_ci = unname(quantile(test_df[["stat"]], 0.75))
    )
  )
})

test_that("get_confidence_interval works with `type = 'se'`", {
  expect_equal(
    test_df |>
      get_confidence_interval(type = "se", point_estimate = point),
    tibble::tibble(lower_ci = -5.653, upper_ci = 6.603),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  # use equivalent rather than equal as ci has attributes for se and point est
  expect_equal(
    test_df |>
      get_confidence_interval(level = 0.5, type = "se", point_estimate = point),
    tibble::tibble(lower_ci = -1.633, upper_ci = 2.583),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )
})

test_that("get_confidence_interval works with `type = 'bias-corrected'`", {
  expect_equal(
    test_df |>
      get_confidence_interval(
        type = "bias-corrected",
        point_estimate = point
      ),
    tibble::tibble(lower_ci = -4.00, upper_ci = 5),
    tolerance = 1e-3
  )

  expect_equal(
    test_df |>
      get_confidence_interval(
        level = 0.5,
        type = "bias-corrected",
        point_estimate = point
      ),
    tibble::tibble(lower_ci = 0, upper_ci = 4.0007),
    tolerance = 1e-3
  )
})

test_that("get_confidence_interval supports data frame `point_estimate`", {
  point_df <- data.frame(p = point)

  expect_equal(
    test_df |> get_confidence_interval(type = "se", point_estimate = point),
    test_df |> get_confidence_interval(type = "se", point_estimate = point_df),
    tolerance = eps
  )
  expect_equal(
    test_df |>
      get_confidence_interval(type = "bias-corrected", point_estimate = point),
    test_df |>
      get_confidence_interval(
        type = "bias-corrected",
        point_estimate = point_df
      ),
    tolerance = eps
  )
})

test_that("get_confidence_interval messages with no explicit `level`", {
  withr::local_envvar(SUPPRESS_INFER_MESSAGES = "false")
  expect_snapshot(res_ <- get_confidence_interval(test_df))
  expect_silent(get_confidence_interval(test_df, level = 0.95))
  expect_silent(get_confidence_interval(test_df, 0.95))
})

test_that("get_confidence_interval checks input", {
  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(type = "other")
  )
  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(level = 1.2)
  )

  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(point_estimate = "a")
  )
  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(type = "se", point_estimate = "a")
  )
  expect_snapshot(
    error = TRUE,
    test_df |>
      get_confidence_interval(
        type = "se",
        point_estimate = data.frame(p = "a")
      )
  )

  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(type = "se")
  )
  expect_snapshot(
    error = TRUE,
    test_df |> get_confidence_interval(type = "bias-corrected")
  )
})


test_that("get_confidence_interval can handle fitted objects", {
  # generate example objects
  set.seed(1)

  null_fits <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    hypothesize(null = "independence") |>
    generate(reps = 10, type = "permute") |>
    fit()

  obs_fit <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    fit()

  # check each ci type
  expect_equal(
    get_confidence_interval(null_fits, point_estimate = obs_fit, level = .95),
    structure(
      list(
        term = c("age", "collegedegree", "intercept"),
        lower_ci = c(-0.2139, -6.6020, 36.4537),
        upper_ci = c(0.1064, 8.7479, 50.8005)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_equal(
    get_confidence_interval(
      null_fits,
      point_estimate = obs_fit,
      level = .95,
      type = "se"
    ),
    structure(
      list(
        term = c("age", "collegedegree", "intercept"),
        lower_ci = c(-0.3809, -13.6182, 36.8694),
        upper_ci = c(0.1124, 6.1680, 59.1752)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  expect_equal(
    get_confidence_interval(
      null_fits,
      point_estimate = obs_fit,
      level = .95,
      type = "bias-corrected"
    ),
    structure(
      list(
        term = c("age", "collegedegree", "intercept"),
        lower_ci = c(-0.2177, -7.1506, 37.2941),
        upper_ci = c(0.0806, 1.9707, 51.0512)
      ),
      row.names = c(NA, -3L),
      class = c("tbl_df", "tbl", "data.frame")
    ),
    tolerance = 1e-3,
    ignore_attr = TRUE
  )

  # errors out when it ought to
  obs_fit_2 <- gss[1:50, ] |>
    specify(hours ~ age) |>
    fit()

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(null_fits, point_estimate = obs_fit_2, level = .95)
  )

  obs_fit_3 <-
    obs_fit_2 <- gss[1:50, ] |>
      specify(year ~ age + college) |>
      fit()

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(null_fits, point_estimate = obs_fit_3, level = .95)
  )
})

test_that("get_confidence_interval can handle bad args with fitted objects", {
  set.seed(1)

  null_fits <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    hypothesize(null = "independence") |>
    generate(reps = 10, type = "permute") |>
    fit()

  obs_fit <- gss[1:50, ] |>
    specify(hours ~ age + college) |>
    fit()

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(null_fits, point_estimate = "boop", level = .95)
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_fits,
      point_estimate = obs_fit$estimate,
      level = .95
    )
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(obs_fit, point_estimate = null_fits, level = .95)
  )
})

test_that("theoretical CIs align with simulation-based (mean)", {
  x_bar <- gss |>
    specify(response = hours) |>
    calculate(stat = "mean")

  set.seed(1)

  null_dist <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    generate(reps = 1e3, type = "bootstrap") |>
    calculate(stat = "mean")

  null_dist_theory <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    assume(distribution = "t")

  expect_equal(
    get_confidence_interval(
      null_dist,
      .95,
      type = "se",
      point_estimate = x_bar
    ),
    get_confidence_interval(
      null_dist_theory,
      .95,
      type = "se",
      point_estimate = x_bar
    ),
    tolerance = .2
  )
})

test_that("theoretical CIs align with simulation-based (prop)", {
  p_hat <- gss |>
    specify(response = sex, success = "female") |>
    calculate(stat = "prop")

  set.seed(1)

  null_dist <- gss |>
    specify(response = sex, success = "female") |>
    hypothesize(null = "point", p = .5) |>
    generate(reps = 1e3, type = "draw") |>
    calculate(stat = "prop")

  null_dist_theory <- gss |>
    specify(response = sex, success = "female") |>
    assume(distribution = "z")

  expect_equal(
    get_confidence_interval(
      null_dist,
      .95,
      type = "se",
      point_estimate = p_hat
    ),
    get_confidence_interval(
      null_dist_theory,
      .95,
      type = "se",
      point_estimate = p_hat
    ),
    tolerance = .05
  )
})

test_that("theoretical CIs align with simulation-based (diff in means)", {
  diff_bar <- gss |>
    specify(age ~ college) |>
    calculate(stat = "diff in means", order = c("degree", "no degree"))

  set.seed(1)

  null_dist <- gss |>
    specify(age ~ college) |>
    hypothesize(null = "independence") |>
    generate(reps = 3e3, type = "permute") |>
    calculate(stat = "diff in means", order = c("degree", "no degree"))

  null_dist_theory <- gss |>
    specify(age ~ college) |>
    assume(distribution = "t")

  expect_equal(
    get_confidence_interval(
      null_dist,
      .95,
      type = "se",
      point_estimate = diff_bar
    ),
    get_confidence_interval(
      null_dist_theory,
      .95,
      type = "se",
      point_estimate = diff_bar
    ),
    tolerance = .15
  )
})

test_that("theoretical CIs align with simulation-based (diff in props)", {
  diff_hat <- gss |>
    specify(college ~ sex, success = "no degree") |>
    calculate(stat = "diff in props", order = c("female", "male"))

  set.seed(1)

  null_dist <- gss |>
    specify(college ~ sex, success = "no degree") |>
    hypothesize(null = "independence") |>
    generate(reps = 1e3, type = "permute") |>
    calculate(stat = "diff in props", order = c("female", "male"))

  null_dist_theory <- gss |>
    specify(college ~ sex, success = "no degree") |>
    assume(distribution = "z")

  expect_equal(
    get_confidence_interval(
      null_dist,
      .95,
      type = "se",
      point_estimate = diff_hat
    ),
    get_confidence_interval(
      null_dist_theory,
      .95,
      type = "se",
      point_estimate = diff_hat
    ),
    tolerance = .001
  )
})

test_that("theoretical CIs check arguments properly", {
  x_bar <- gss |>
    specify(response = hours) |>
    calculate(stat = "mean")

  null_dist_theory <- gss |>
    specify(age ~ college) |>
    assume(distribution = "t")

  # check that type is handled correctly
  expect_equal(
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      point_estimate = x_bar
    ),
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      type = "se",
      point_estimate = x_bar
    )
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      type = "percentile",
      point_estimate = x_bar
    )
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      type = "boop",
      point_estimate = x_bar
    )
  )

  # check that point estimate hasn't been post-processed
  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      point_estimate = dplyr::pull(x_bar)
    )
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      point_estimate = x_bar$stat
    )
  )

  # check that statistics are implemented
  obs_t <- gss |>
    specify(response = hours) |>
    hypothesize(null = "point", mu = 40) |>
    calculate(stat = "t")

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      point_estimate = obs_t
    )
  )

  # check that stat and distribution align
  p_hat <- gss |>
    specify(response = sex, success = "female") |>
    calculate(stat = "prop")

  null_dist_z <- gss |>
    specify(response = sex, success = "female") |>
    assume(distribution = "z")

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_theory,
      level = .95,
      point_estimate = p_hat
    )
  )

  expect_snapshot(
    error = TRUE,
    get_confidence_interval(
      null_dist_z,
      level = .95,
      point_estimate = x_bar
    )
  )
})

test_that("handles missing values gracefully (#520)", {
  data <- data.frame(
    prop = seq(0, 1, length.out = 10),
    group = rep(c("a", "b"), each = 5L)
  )

  set.seed(1)
  boot_dist <-
    data |>
    specify(prop ~ group) |>
    hypothesize(null = "independence") |>
    generate(reps = 1000, type = "bootstrap") |>
    calculate(stat = "diff in medians", order = c("b", "a"))

  expect_snapshot(res <- get_confidence_interval(boot_dist, .95))

  expect_s3_class(res, "data.frame")
})
