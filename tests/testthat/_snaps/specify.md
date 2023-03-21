# data argument

    Code
      specify(blah ~ cyl)
    Condition
      Error in `specify()`:
      ! `x` must be 'data.frame', not 'language'.

---

    Code
      specify(1:3)
    Condition
      Error in `specify()`:
      ! `x` must be 'data.frame', not 'integer'.

---

    Code
      specify(mtcars_df, mtcars_df$mpg)
    Condition
      Error in `specify()`:
      ! The first unnamed argument must be a formula. You passed in 'double'. Did you forget to name one or more arguments?

# response and explanatory arguments

    Code
      specify(mtcars_df, response = blah)
    Condition
      Error in `specify()`:
      ! The response variable `blah` cannot be found in this dataframe.

---

    Code
      specify(mtcars_df, response = "blah")
    Condition
      Error in `specify()`:
      ! The response should be a bare variable name (not a string in quotation marks).

---

    Code
      specify(mtcars_df, formula = mpg ~ blah)
    Condition
      Error in `specify()`:
      ! The explanatory variable `blah` cannot be found in this dataframe.

---

    Code
      specify(mtcars_df, blah2 ~ cyl)
    Condition
      Error in `specify()`:
      ! The response variable `blah2` cannot be found in this dataframe.

---

    Code
      specify(mtcars_df)
    Condition
      Error in `specify()`:
      ! Please supply a response variable that is not `NULL`.

---

    Code
      specify(mtcars_df, formula = mpg ~ mpg)
    Condition
      Error in `specify()`:
      ! The response and explanatory variables must be different from one another.

---

    Code
      specify(mtcars_df, formula = "mpg" ~ cyl)
    Condition
      Error in `specify()`:
      ! The response should be a bare variable name (not a string in quotation marks).

---

    Code
      specify(mtcars_df, formula = mpg ~ "cyl")
    Condition
      Error in `specify()`:
      ! The explanatory should be a bare variable name (not a string in quotation marks).

---

    Code
      specify(mtcars_df, formula = NULL ~ cyl)
    Condition
      Error in `specify()`:
      ! Please supply a response variable that is not `NULL`.

# success argument

    Code
      specify(mtcars_df, response = vs, success = 1)
    Condition
      Error in `specify()`:
      ! `success` must be a string.

---

    Code
      specify(mtcars_df, response = vs, success = "bogus")
    Condition
      Error in `specify()`:
      ! bogus is not a valid level of vs.

---

    Code
      specify(mtcars_df, response = mpg, success = "1")
    Condition
      Error in `specify()`:
      ! `success` should only be specified if the response is a categorical variable.

---

    Code
      specify(mtcars_df, response = cyl, success = "4")
    Condition
      Error in `specify()`:
      ! `success` can only be used if the response has two levels. `filter()` can reduce a variable to two levels.

---

    Code
      specify(mtcars_df, response = am)
    Condition
      Error in `specify()`:
      ! A level of the response variable `am` needs to be specified for the `success` argument in `specify()`.

# formula argument is a formula

    Code
      specify(mtcars_df, formula = "vs", success = 1)
    Condition
      Error in `specify()`:
      ! The first unnamed argument must be a formula. You passed in 'character'. Did you forget to name one or more arguments?

---

    Code
      specify(mtcars, am, success = "1")
    Condition
      Error in `specify()`:
      ! The argument you passed in for the formula does not exist.
      * Were you trying to pass in an unquoted column name?
      * Did you forget to name one or more arguments?

---

    Code
      specify(mtcars, response = am, "1")
    Condition
      Error in `specify()`:
      ! The first unnamed argument must be a formula. You passed in 'character'. Did you forget to name one or more arguments?

# is_complete works

    Code
      res_ <- specify(some_missing, response = vec)
    Condition
      Warning:
      Removed 1 rows containing missing values.

# specify messages when dropping unused levels

    Code
      res_ <- gss %>% dplyr::filter(partyid %in% c("rep", "dem")) %>% specify(age ~
        partyid)
    Message
      Dropping unused factor levels c("ind", "other", "DK") from the supplied explanatory variable 'partyid'.

---

    Code
      res_ <- gss %>% dplyr::filter(partyid %in% c("rep", "dem")) %>% specify(
        partyid ~ age)
    Message
      Dropping unused factor levels c("ind", "other", "DK") from the supplied response variable 'partyid'.

---

    Code
      res_ <- gss %>% dplyr::filter(partyid %in% c("rep", "dem")) %>% specify(
        partyid ~ NULL)
    Message
      Dropping unused factor levels c("ind", "other", "DK") from the supplied response variable 'partyid'.

