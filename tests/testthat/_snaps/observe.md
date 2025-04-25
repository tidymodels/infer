# observe() output is the same as the old wrappers

    Code
      res_wrap <- chisq_stat(gss_tbl, college ~ partyid)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      res_wrap_2 <- t_stat(gss_tbl, hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

