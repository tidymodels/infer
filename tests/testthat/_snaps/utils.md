# stop_glue handles `NULL`

    Code
      stop_glue("Hello {null_val}", "!")
    Condition
      Error:
      ! Hello NULL!

# check_type works

    Code
      check_type(x_var, is.character)
    Condition
      Error:
      ! `x_var` must be 'character', not 'integer'.

---

    Code
      check_type(x_var, is.character, "symbolic")
    Condition
      Error:
      ! `x_var` must be 'symbolic', not 'integer'.

---

    Code
      check_type(x_df, is.logical)
    Condition
      Error:
      ! `x_df` must be 'logical', not 'data.frame'.

# check_type allows custom name for `x`

    Code
      check_type(input, is.numeric, x_name = "aaa")
    Condition
      Error:
      ! `aaa` must be 'numeric', not 'character'.

# check_type allows extra arguments for `predicate`

    Code
      check_type(1, is_geq, min_val = 2)
    Condition
      Error:
      ! `1` must be 'geq', not 'double'.

# check_type allows formula `predicate`

    Code
      check_type("a", ~ is.numeric(.))
    Condition
      Error:
      ! `"a"` must be '~is.numeric(.)', not 'character'.

# hypothesize errors out when x isn't a dataframe

    Code
      hypothesize(c(1, 2, 3), null = "point")
    Condition
      Error:
      ! x must be a data.frame or tibble

