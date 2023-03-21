# observe() output is the same as the old wrappers

    Code
      res_wrap <- gss_tbl %>% chisq_stat(college ~ partyid)
    Condition
      Warning:
      The chisq_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

---

    Code
      res_wrap_2 <- gss_tbl %>% t_stat(hours ~ sex, order = c("male", "female"))
    Condition
      Warning:
      The t_stat() wrapper has been deprecated in favor of the more general observe(). Please use that function instead.

