# assume errors with bad arguments

    Code
      gss %>% specify(age ~ college) %>% hypothesize(null = "independence") %>%
        assume("boop", nrow(gss) - 1)
    Condition
      Error:
      ! The distribution argument must be one of "Chisq", "F", "t", or "z".

---

    Code
      gss %>% specify(age ~ college) %>% hypothesize(null = "independence") %>%
        assume("t", c(nrow(gss) - 1, 2))
    Condition
      Error:
      ! A T distribution requires 1 degrees of freedom argument, but 2 were supplied.

---

    Code
      gss %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        assume("F", nrow(gss) - 1)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error:
      ! An F distribution requires 2 degrees of freedom arguments, but 1 was supplied.

---

    Code
      gss %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        assume("F", "boop")
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error:
      ! `assume()` expects the `df` argument to be a numeric vector, but you supplied a character object.

---

    Code
      gss %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        assume("F", nrow(gss) - 1, 1)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error:
      ! `assume()` ignores the dots `...` argument, though the argument `list(1)` was supplied. Did you forget to concatenate the `df` argument with `c()`?

---

    Code
      gss %>% specify(age ~ partyid) %>% hypothesize(null = "independence") %>%
        assume("F", nrow(gss) - 1, 1, 2)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error:
      ! `assume()` ignores the dots `...` argument, though the arguments `list(1, 2)` were supplied. Did you forget to concatenate the `df` argument with `c()`?

---

    Code
      gss %>% specify(age ~ finrela) %>% hypothesize(null = "independence") %>%
        assume("t", nrow(gss) - 1)
    Condition
      Error:
      ! The supplied distribution "t" is not well-defined for a numeric response variable (age) and a multinomial categorical explanatory variable (finrela).

---

    Code
      gss %>% specify(age ~ finrela) %>% hypothesize(null = "independence") %>%
        assume("z", nrow(gss) - 1)
    Condition
      Error:
      ! The supplied distribution "z" is not well-defined for a numeric response variable (age) and a multinomial categorical explanatory variable (finrela).

---

    Code
      gss %>% specify(age ~ NULL) %>% hypothesize(null = "point", mu = 40) %>% assume(
        "z", nrow(gss) - 1)
    Condition
      Error:
      ! The supplied distribution "z" is not well-defined for a numeric response variable (age) and no explanatory variable.

---

    Code
      gss %>% assume("z", nrow(gss) - 1)
    Condition
      Error:
      ! The `x` argument must be the output of a core infer function, likely `specify()` or `hypothesize()`.

---

    Code
      "boop" %>% assume("z", nrow(gss) - 1)
    Condition
      Error:
      ! The `x` argument must be the output of a core infer function, likely `specify()` or `hypothesize()`.

