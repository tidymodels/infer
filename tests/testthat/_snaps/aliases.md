# old aliases produce informative error

    Code
      res <- p_value(gss_calc, obs_stat = -0.2, direction = "right")
    Condition
      Error:
      ! `conf_int()` was deprecated in infer 0.4.0 and is now defunct.
      i Please use `get_p_value()` instead.

---

    Code
      res_ <- conf_int(gss_permute)
    Condition
      Error:
      ! `conf_int()` was deprecated in infer 0.4.0 and is now defunct.
      i Please use `get_confidence_interval()` instead.

