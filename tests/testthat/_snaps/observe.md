# observe() output is the same as the old wrappers

    Code
      res_wrap <- gss_tbl %>% chisq_stat(college ~ partyid)
    Condition
      Warning:
      `chisq_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

---

    Code
      res_wrap_2 <- gss_tbl %>% t_stat(hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      `t_stat()` was deprecated in infer 1.0.0.
      i Please use `observe()` instead.

