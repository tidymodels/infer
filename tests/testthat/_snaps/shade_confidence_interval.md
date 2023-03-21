# shade_confidence_interval throws errors and warnings

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(c(1, 2, 3))
    Condition
      Warning:
      Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector. Using the first two entries as the `endpoints`.

---

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(data.frame(x = 1))
    Condition
      Error:
      ! Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector.

---

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(c(-1, 1), color = "x")
    Condition
      Error:
      ! `color` must be 'color string', not 'character'.

---

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(c(-1, 1), fill = "x")
    Condition
      Error:
      ! `fill` must be 'color string', not 'character'.

---

    Code
      res_ <- gss_viz_sim %>% shade_confidence_interval(c(-1, 1))
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_confidence_interval()` (using `%>%`) rather than adding the result of `shade_confidence_interval()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_confidence_interval(endpoints = c(-1, 1))
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_confidence_interval()` (using `%>%`) rather than adding the result of `shade_confidence_interval()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_ci(c(-1, 1))
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_ci()` (using `%>%`) rather than adding the result of `shade_ci()` as a layer with `+`. Consider changing`%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_ci(endpoints = c(-1, 1))
    Condition
      Error:
      ! It looks like you piped the result of `visualize()` into `shade_ci()` (using `%>%`) rather than adding the result of `shade_ci()` as a layer with `+`. Consider changing`%>%` to `+`.

