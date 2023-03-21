# old aliases produce warning

    Code
      res <- gss_calc %>% p_value(obs_stat = -0.2, direction = "right") %>% dplyr::pull()
    Condition
      Warning:
      'p_value' is deprecated.
      Use 'get_p_value' instead.
      See help("Deprecated")

---

    Code
      res_ <- gss_permute %>% conf_int()
    Condition
      Warning:
      'conf_int' is deprecated.
      Use 'get_confidence_interval' instead.
      See help("Deprecated")

