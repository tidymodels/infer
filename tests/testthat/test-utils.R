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
