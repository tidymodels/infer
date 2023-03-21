# `rep_sample_n` checks input

    Code
      rep_sample_n("a", size = 1)
    Condition
      Error:
      ! `tbl` must be 'data.frame', not 'character'.

---

    Code
      rep_sample_n(population, size = "a")
    Condition
      Error:
      ! `size` must be 'single non-negative number', not 'character'.

---

    Code
      rep_sample_n(population, size = 1:2)
    Condition
      Error:
      ! `size` must be 'single non-negative number', not 'integer'.

---

    Code
      rep_sample_n(population, size = -1)
    Condition
      Error:
      ! `size` must be 'single non-negative number', not 'double'.

---

    Code
      rep_sample_n(population, size = 1, replace = "a")
    Condition
      Error:
      ! `replace` must be 'TRUE or FALSE', not 'character'.

---

    Code
      rep_sample_n(population, size = 1, reps = "a")
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'character'.

---

    Code
      rep_sample_n(population, size = 1, reps = 1:2)
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'integer'.

---

    Code
      rep_sample_n(population, size = 1, reps = 0.5)
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'double'.

---

    Code
      rep_sample_n(population, size = 1, prob = "a")
    Condition
      Error:
      ! `prob` must be 'numeric vector with length `nrow(tbl)` = 5', not 'character'.

---

    Code
      rep_sample_n(population, size = 1, prob = c(0.1, 0.9))
    Condition
      Error:
      ! `prob` must be 'numeric vector with length `nrow(tbl)` = 5', not 'double'.

# `rep_sample_n` gives error on big sample size if `replace=FALSE`

    Code
      rep_sample_n(population, size = n_population * 2)
    Condition
      Error:
      ! Asked sample size (10) is bigger than number of rows in data (5) while `replace` is FALSE. Use `replace = TRUE`.

# `rep_slice_sample` checks input

    Code
      rep_slice_sample("a", n = 1)
    Condition
      Error:
      ! `.data` must be 'data.frame', not 'character'.

---

    Code
      rep_slice_sample(population, n = "a")
    Condition
      Error:
      ! `n` must be 'single non-negative number', not 'character'.

---

    Code
      rep_slice_sample(population, n = 1:2)
    Condition
      Error:
      ! `n` must be 'single non-negative number', not 'integer'.

---

    Code
      rep_slice_sample(population, n = -1)
    Condition
      Error:
      ! `n` must be 'single non-negative number', not 'double'.

---

    Code
      rep_slice_sample(population, prop = "a")
    Condition
      Error:
      ! `prop` must be 'single non-negative number', not 'character'.

---

    Code
      rep_slice_sample(population, prop = 1:2)
    Condition
      Error:
      ! `prop` must be 'single non-negative number', not 'integer'.

---

    Code
      rep_slice_sample(population, prop = -1)
    Condition
      Error:
      ! `prop` must be 'single non-negative number', not 'double'.

---

    Code
      rep_slice_sample(population, n = 1, prop = 0.5)
    Condition
      Error:
      ! Please supply exactly one of the `n` or `prop` arguments.

---

    Code
      rep_slice_sample(population, n = 1, replace = "a")
    Condition
      Error:
      ! `replace` must be 'TRUE or FALSE', not 'character'.

---

    Code
      rep_slice_sample(population, n = 1, weight_by = "a")
    Condition
      Error:
      ! `weight_by` must be 'a numeric vector with length `nrow(.data)` = 5 or an unquoted column name', not 'character'.

---

    Code
      rep_slice_sample(population, n = 1, weight_by = c(0.1, 0.9))
    Condition
      Error:
      ! `weight_by` must be 'a numeric vector with length `nrow(.data)` = 5 or an unquoted column name', not 'double'.

---

    Code
      rep_slice_sample(population, n = 1, weight_by = wts)
    Condition
      Error:
      ! The column `wts` provided to the `weight_by` argument is not in the supplied data.

---

    Code
      rep_slice_sample(population, n = 1, reps = "a")
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'character'.

---

    Code
      rep_slice_sample(population, n = 1, reps = 1:2)
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'integer'.

---

    Code
      rep_slice_sample(population, n = 1, reps = 0.5)
    Condition
      Error:
      ! `reps` must be 'single number not less than 1', not 'double'.

# `rep_slice_sample` warns on big sample size if `replace = FALSE`

    Code
      out <- rep_slice_sample(population, n = n_population * 2, reps = 1)
    Condition
      Warning:
      Asked sample size (10) is bigger than number of rows in data (5) while `replace` is FALSE. Using number of rows as sample size.

---

    Code
      out <- rep_slice_sample(population, prop = 2, reps = 1)
    Condition
      Warning:
      Asked sample size (10) is bigger than number of rows in data (5) while `replace` is FALSE. Using number of rows as sample size.

