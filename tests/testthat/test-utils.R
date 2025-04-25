test_that("append_infer_class works", {
  expect_equal(
    class(append_infer_class(structure("a", class = "b"))),
    c("infer", "b")
  )
  expect_equal(
    class(append_infer_class(structure("a", class = c("infer", "b")))),
    c("infer", "b")
  )
})

null_val <- NULL

test_that("is_single_number works", {
  # Basic usage
  expect_true(is_single_number(1))
  expect_true(is_single_number(1L))
  expect_false(is_single_number("a"))
  expect_false(is_single_number(1:2))

  # Infinity and `NA` are not allowed
  expect_false(is_single_number(Inf))
  expect_false(is_single_number(-Inf))
  expect_false(is_single_number(NA_real_))

  # Using boundaries
  expect_true(is_single_number(1, min_val = -10))
  expect_false(is_single_number(1, min_val = 10))

  expect_true(is_single_number(1, max_val = 10))
  expect_false(is_single_number(1, max_val = -10))

  expect_true(is_single_number(1, min_val = -10, max_val = 10))
  expect_false(is_single_number(1, min_val = -10, max_val = 0))
  expect_false(is_single_number(1, min_val = 10, max_val = 100))

  # Using boundary inclusivity
  ## Inclusive by default
  expect_true(is_single_number(1, min_val = 1))
  expect_true(is_single_number(1, max_val = 1))

  expect_false(is_single_number(1, min_val = 1, include_min_val = FALSE))
  expect_false(is_single_number(1, max_val = 1, include_max_val = FALSE))
})

test_that("is_truefalse works", {
  expect_true(is_truefalse(TRUE))
  expect_true(is_truefalse(FALSE))
  expect_false(is_truefalse(c(TRUE, TRUE)))
  expect_false(is_truefalse("a"))
  expect_false(is_truefalse(1L))
})

test_that("check_type works", {
  x_var <- 1L

  expect_silent(check_type(x_var, is.integer))

  expect_snapshot(error = TRUE, check_type(x_var, is.character))
  expect_snapshot(error = TRUE, check_type(x_var, is.character, "symbolic"))

  x_df <- data.frame(x = TRUE)
  expect_silent(check_type(x_df, is.data.frame))
  expect_snapshot(error = TRUE, check_type(x_df, is.logical))
})

test_that("check_type allows `NULL`", {
  input <- NULL
  expect_silent(check_type(input, is.numeric, allow_null = TRUE))
})

test_that("check_type allows custom name for `x`", {
  input <- "a"
  expect_snapshot(error = TRUE, check_type(input, is.numeric, x_name = "aaa"))
})

test_that("check_type allows extra arguments for `predicate`", {
  is_geq <- function(x, min_val) {
    x >= min_val
  }
  expect_silent(check_type(1, is_geq, min_val = 0))
  expect_snapshot(error = TRUE, check_type(1, is_geq, min_val = 2))
})

test_that("check_type allows formula `predicate`", {
  expect_silent(check_type(1, ~ is.numeric(.) && (. > 0)))

  # By default type should be inferred as the whole formula
  expect_snapshot(error = TRUE, check_type("a", ~ is.numeric(.)))
})


test_that("get_type works", {
  expect_equal(get_type(data.frame(x = 1)), "data.frame")
  expect_equal(get_type(list(x = 1)), "list")
  expect_equal(get_type(TRUE), "logical")
})

test_that("c_dedupl returns input when unnamed", {
  expect_equal(c_dedupl(c(1, 2, 3)), c(1, 2, 3))
})

test_that("hypothesize errors out when x isn't a dataframe", {
  expect_snapshot(error = TRUE, hypothesize(c(1, 2, 3), null = "point"))
})

test_that("p_null supplies appropriate params", {
  expect_equal(
    gss |> specify(partyid ~ NULL) |> p_null(),
    c(p.dem = 0.2, p.ind = 0.2, p.rep = 0.2, p.other = 0.2, p.DK = 0.2)
  )
})

test_that("variables are standardized as expected", {
  gss_types <-
    gss |>
    dplyr::mutate(
      age = as.integer(age),
      is_dem = dplyr::if_else(partyid == "dem", TRUE, FALSE),
      finrela = as.character(finrela)
    )

  gss_std <- standardize_variable_types(gss_types)

  expect_true(inherits(gss_types$age, "integer"))
  expect_true(inherits(gss_types$finrela, "character"))
  expect_true(inherits(gss_types$income, "ordered"))
  expect_true(inherits(gss_types$college, "factor"))
  expect_true(inherits(gss_types$is_dem, "logical"))

  expect_null(levels(gss_types$is_dem))

  expect_true(inherits(gss_std$age, "numeric"))
  expect_true(inherits(gss_std$finrela, "factor"))
  expect_true(inherits(gss_std$income, "factor"))
  expect_true(inherits(gss_std$college, "factor"))
  expect_true(inherits(gss_std$is_dem, "factor"))

  expect_equal(levels(gss_std$is_dem), c("TRUE", "FALSE"))
})

test_that("group_by_replicate() helper returns correct results", {
  reps <- 500
  nrow_gss <- nrow(gss)

  gss_gen <-
    gss |>
    specify(age ~ college) |>
    hypothesize(null = "independence") |>
    generate(reps = reps, type = "permute") |>
    dplyr::ungroup()

  expect_equal(
    dplyr::group_by(gss_gen, replicate),
    group_by_replicate(gss_gen, reps, nrow_gss)
  )
})
