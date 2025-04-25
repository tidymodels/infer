# shade_p_value throws errors

    Code
      gss_viz_sim + shade_p_value("a", "right")
    Condition
      Error in `shade_p_value()`:
      ! `obs_stat` must be 'numeric', not 'character'.

---

    Code
      gss_viz_sim + shade_p_value(1, 1)
    Condition
      Error in `shade_p_value()`:
      ! `direction` must be 'character', not 'double'.

---

    Code
      gss_viz_sim + shade_p_value(1, "right", color = "x")
    Condition
      Error in `shade_p_value()`:
      ! `color` must be 'color string', not 'character'.

---

    Code
      gss_viz_sim + shade_p_value(1, "right", fill = "x")
    Condition
      Error in `shade_p_value()`:
      ! `fill` must be 'color string', not 'character'.

---

    Code
      shade_p_value(gss_viz_sim, 1, "right")
    Condition
      Error in `shade_p_value()`:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` rather than adding the result of `shade_p_value()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

---

    Code
      shade_p_value(gss_viz_sim, obs_stat = 1)
    Condition
      Error in `shade_p_value()`:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` rather than adding the result of `shade_p_value()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

---

    Code
      shade_p_value(gss_viz_sim, obs_stat = 1, direction = "right")
    Condition
      Error in `shade_p_value()`:
      ! It looks like you piped the result of `visualize()` into `shade_p_value()` rather than adding the result of `shade_p_value()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

---

    Code
      shade_pvalue(gss_viz_sim, 1, "right")
    Condition
      Error in `shade_pvalue()`:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` rather than adding the result of `shade_pvalue()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

---

    Code
      shade_pvalue(gss_viz_sim, obs_stat = 1)
    Condition
      Error in `shade_pvalue()`:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` rather than adding the result of `shade_pvalue()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

---

    Code
      shade_pvalue(gss_viz_sim, obs_stat = 1, direction = "right")
    Condition
      Error in `shade_pvalue()`:
      ! It looks like you piped the result of `visualize()` into `shade_pvalue()` rather than adding the result of `shade_pvalue()` as a layer with `+`.
      i Consider changing `|>` (or `%>%`) to `+`.

