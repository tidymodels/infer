# shade_p_value throws errors

    Code
      gss_viz_sim + shade_p_value("a", "right")
    Condition
      Error:
      ! `obs_stat` must be 'numeric', not 'character'.

---

    Code
      gss_viz_sim + shade_p_value(1, 1)
    Condition
      Error:
      ! `direction` must be 'character', not 'double'.

---

    Code
      gss_viz_sim + shade_p_value(1, "right", color = "x")
    Condition
      Error:
      ! `color` must be 'color string', not 'character'.

---

    Code
      gss_viz_sim + shade_p_value(1, "right", fill = "x")
    Condition
      Error:
      ! `fill` must be 'color string', not 'character'.

---

    Code
      gss_viz_sim %>% shade_p_value(1, "right")
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` (using `%>%`) rather than adding the result of `shade_p_value()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      gss_viz_sim %>% shade_p_value(obs_stat = 1)
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` (using `%>%`) rather than adding the result of `shade_p_value()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      gss_viz_sim %>% shade_p_value(obs_stat = 1, direction = "right")
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` (using `%>%`) rather than adding the result of `shade_p_value()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      gss_viz_sim %>% shade_pvalue(1, "right")
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` (using `%>%`) rather than adding the result of `shade_pvalue()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      gss_viz_sim %>% shade_pvalue(obs_stat = 1)
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` (using `%>%`) rather than adding the result of `shade_pvalue()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      gss_viz_sim %>% shade_pvalue(obs_stat = 1, direction = "right")
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` (using `%>%`) rather than adding the result of `shade_pvalue()` as a layer with `+`. Consider changing`%>%` to `+`.

