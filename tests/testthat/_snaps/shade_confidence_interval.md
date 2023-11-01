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
      Error in `shade_confidence_interval()`:
      ! Expecting `endpoints` to be a 1 x 2 data frame or 2 element vector.

---

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(c(-1, 1), color = "x")
    Condition
      Error in `shade_confidence_interval_term()`:
      ! `color` must be 'color string', not 'character'.

---

    Code
      res_ <- gss_viz_sim + shade_confidence_interval(c(-1, 1), fill = "x")
    Condition
      Error in `shade_confidence_interval_term()`:
      ! `fill` must be 'color string', not 'character'.

---

    Code
      res_ <- gss_viz_sim %>% shade_confidence_interval(c(-1, 1))
    Condition
      Error in `shade_confidence_interval()`:
      ! It looks like you piped the result of `visualize()` into `shade_confidence_interval()` (using `%>%`) rather than adding the result of `shade_confidence_interval()` as a layer with `+`.
      i Consider changing `%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_confidence_interval(endpoints = c(-1, 1))
    Condition
      Error in `shade_confidence_interval()`:
      ! It looks like you piped the result of `visualize()` into `shade_confidence_interval()` (using `%>%`) rather than adding the result of `shade_confidence_interval()` as a layer with `+`.
      i Consider changing `%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_ci(c(-1, 1))
    Condition
      Error in `shade_ci()`:
      ! It looks like you piped the result of `visualize()` into `shade_ci()` (using `%>%`) rather than adding the result of `shade_ci()` as a layer with `+`.
      i Consider changing `%>%` to `+`.

---

    Code
      res_ <- gss_viz_sim %>% shade_ci(endpoints = c(-1, 1))
    Condition
      Error in `shade_ci()`:
      ! It looks like you piped the result of `visualize()` into `shade_ci()` (using `%>%`) rather than adding the result of `shade_ci()` as a layer with `+`.
      i Consider changing `%>%` to `+`.

