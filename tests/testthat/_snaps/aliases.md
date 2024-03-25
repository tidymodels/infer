# old aliases produce informative error

    Code
      res <- gss_calc %>% p_value(obs_stat = -0.2, direction = "right")
    Condition
      Error:
      ! `conf_int()` was deprecated in infer 0.4.0 and is now defunct.
      i Please use `get_p_value()` instead.

---

    Code
      res_ <- gss_permute %>% conf_int()
    Condition
      Error:
      ! `conf_int()` was deprecated in infer 0.4.0 and is now defunct.
      i Please use `get_confidence_interval()` instead.

