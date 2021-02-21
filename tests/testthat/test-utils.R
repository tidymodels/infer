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
