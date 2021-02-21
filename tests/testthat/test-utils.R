context("utils")

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

test_that("stop_glue handles `NULL`", {
  expect_error(stop_glue("Hello {null_val}", "!"), "NULL")
})

test_that("warning_glue handles `NULL`", {
  expect_warning(warning_glue("Hello {null_val}", "!"), "NULL")
})

test_that("message_glue handles `NULL`", {
  expect_message(message_glue("Hello {null_val}", "!"), "NULL")
})

test_that("glue_null works", {
  adj <- "quick"

  expect_equal(
    glue_null(
      "The {adj} brown {null_val} jumps ", "over the lazy {NULL}."
    ),
    "The quick brown NULL jumps over the lazy NULL."
  )

  expect_equal(
    glue_null("The {adj}", "brown", .sep = "-"),
    "The quick-brown"
  )
})

test_that("check_type works", {
  x_var <- 1L

  expect_silent(check_type(x_var, is.integer))

  expect_error(check_type(x_var, is.character), "x_var.*character.*integer")
  expect_error(
    check_type(x_var, is.character, "symbolic"), "x_var.*symbolic.*integer"
  )

  x_df <- data.frame(x = TRUE)
  expect_silent(check_type(x_df, is.data.frame))
  expect_error(
    check_type(x_df, is.logical), "x_df.*logical.*data\\.frame"
  )
})

test_that("check_type allows `NULL`", {
  input <- NULL
  expect_silent(check_type(input, is.numeric, allow_null = TRUE))
})

test_that("check_type allows custom name for `x`", {
  input <- "a"
  expect_error(check_type(input, is.numeric, x_name = "aaa"), "^`aaa`")
})

test_that("check_type allows extra arguments for `predicate`", {
  is_geq <- function(x, min_val) {
    x >= min_val
  }
  expect_silent(check_type(1, is_geq, min_val = 0))
  expect_error(check_type(1, is_geq, min_val = 2))
})

test_that("check_type allows formula `predicate`", {
  expect_silent(check_type(1, ~ is.numeric(.) && (. > 0)))

  # By default type should be inferred as the whole formula
  expect_error(check_type("a", ~ is.numeric(.)), "'~is\\.numeric\\(\\.\\)'")
})


test_that("get_type works", {
  expect_equal(get_type(data.frame(x = 1)), "data.frame")
  expect_equal(get_type(list(x = 1)), "list")
  expect_equal(get_type(TRUE), "logical")
})

test_that("c_dedupl returns input when unnamed", {
  expect_equal(c_dedupl(c(1, 2, 3)), c(1, 2, 3))
})

test_that("hypothesize errors out when x isn't a dataframe",
          expect_error(hypothesize(c(1, 2, 3), null = "point"), 
                       "x must be a data.frame or tibble"))

test_that("p_null supplies appropriate params", {
  expect_equal(
    gss %>% specify(partyid ~ NULL) %>% p_null(),
    c(p.dem = 0.2, p.ind = 0.2, p.rep = 0.2, p.other = 0.2, p.DK = 0.2)
  )
})
