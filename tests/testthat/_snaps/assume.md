# assume errors with bad arguments

    Code
      assume(hypothesize(specify(gss, age ~ college), null = "independence"), "boop",
      nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The distribution argument must be one of "Chisq", "F", "t", or "z".

---

    Code
      assume(hypothesize(specify(gss, age ~ college), null = "independence"), "t", c(
        nrow(gss) - 1, 2))
    Condition
      Error in `assume()`:
      ! A T distribution requires 1 degrees of freedom argument, but 2 were supplied.

---

    Code
      assume(hypothesize(specify(gss, age ~ partyid), null = "independence"), "F",
      nrow(gss) - 1)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error in `assume()`:
      ! An F distribution requires 2 degrees of freedom arguments, but 1 was supplied.

---

    Code
      assume(hypothesize(specify(gss, age ~ partyid), null = "independence"), "F",
      "boop")
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error in `assume()`:
      ! `assume()` expects the `df` argument to be a numeric vector, but you supplied a character object.

---

    Code
      assume(hypothesize(specify(gss, age ~ partyid), null = "independence"), "F",
      nrow(gss) - 1, 1)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error in `assume()`:
      ! `assume()` ignores the dots `...` argument, though the argument were supplied.
      i Did you forget to concatenate the `df` argument with `c()`?

---

    Code
      assume(hypothesize(specify(gss, age ~ partyid), null = "independence"), "F",
      nrow(gss) - 1, 1, 2)
    Message
      Dropping unused factor levels DK from the supplied explanatory variable 'partyid'.
    Condition
      Error in `assume()`:
      ! `assume()` ignores the dots `...` argument, though the arguments were supplied.
      i Did you forget to concatenate the `df` argument with `c()`?

---

    Code
      assume(hypothesize(specify(gss, age ~ finrela), null = "independence"), "t",
      nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The supplied distribution "t" is not well-defined for a numeric response variable (age) and a multinomial categorical explanatory variable (finrela).

---

    Code
      assume(hypothesize(specify(gss, age ~ finrela), null = "independence"), "z",
      nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The supplied distribution "z" is not well-defined for a numeric response variable (age) and a multinomial categorical explanatory variable (finrela).

---

    Code
      assume(hypothesize(specify(gss, age ~ NULL), null = "point", mu = 40), "z",
      nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The supplied distribution "z" is not well-defined for a numeric response variable (age) and no explanatory variable.

---

    Code
      assume(gss, "z", nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The `x` argument must be the output of a core infer function, likely `specify()` or `hypothesize()`.

---

    Code
      assume("boop", "z", nrow(gss) - 1)
    Condition
      Error in `assume()`:
      ! The `x` argument must be the output of a core infer function, likely `specify()` or `hypothesize()`.

# assume() handles automatic df gracefully

    Code
      res_ <- assume(hypothesize(specify(gss, response = hours), null = "point", mu = 40),
      "t", nrow(gss) - 2)
    Message
      Message: The supplied `df` argument does not match its expected value. If this is unexpected, ensure that your calculation for `df` is correct (see `assume()` (`?infer::assume()`) for recognized values) or supply `df = NULL` to `assume()`.

