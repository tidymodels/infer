context("utils")

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
