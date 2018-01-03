#' Specify the response and explanatory variables
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param formula a formula with the response variable on the left and the explanatory on the right
#' @param response the variable name in \code{x} that will serve as the response. This is alternative to using the \code{formula} argument.
#' @param explanatory the variable name in \code{x} that will serve as the explanatory variable
#' @param success the name of the level of \code{response} that will be considered a success. Needed for inference on one proportion or a difference in proportions.
#' @importFrom rlang f_lhs
#' @importFrom rlang f_rhs
#' @importFrom dplyr mutate_if select one_of as_tibble
#' @importFrom methods hasArg
#' @export

specify <- function(x, formula, response = NULL, explanatory = NULL, success = NULL) {
  assertive::assert_is_data.frame(x)

  # if(methods::hasArg(formula)) {
  #   assertive::assert_is_formula(formula)
  #   assertive::assert_is_subset(f_lhs(formula), colnames(x))
  #   assertive::assert_is_subset(f_rhs(formula), colnames(x))
  # } else {
  #   response_arg <- as.character(substitute(response))
  #   assertive::assert_is_subset(response_arg, colnames(x))
  #   explanatory_arg <- as.character(substitute(explanatory))
  #   assertive::assert_is_subset(explanatory_arg, colnames(x))
  # }
  #
  # Convert all character variables to be factor variables instead
  x <- dplyr::as_tibble(x) %>% mutate_if(is.character, as.factor)
  #
  # response_col <- rlang::eval_tidy(enquo(response), x)
  # if(is.factor(response_col)) {
  #   assertive::assert_is_subset(success, levels(response_col))
  # }

  if(!methods::hasArg(formula) && !methods::hasArg(response)) {
    stop("Please specify the response variable.")
  }
  if(methods::hasArg(formula)) {
    if(!rlang::is_formula(formula)) {
      stop("The `formula` argument is not recognized as a formula.")
    }
  }

  attr(x, "response")    <- substitute(response)
  attr(x, "explanatory") <- substitute(explanatory)

  if (methods::hasArg(formula)) {
    attr(x, "response")    <- f_lhs(formula)
    attr(x, "explanatory") <- f_rhs(formula)
  }

  if(!as.character(attr(x, "response")) %in% names(x)) {
    stop(paste0("The response variable `", attr(x, "response"), "` cannot be found in this dataframe."))
  }

  if(!is.null(attr(x, "explanatory")) || as.character(attr(x, "explanatory")) == "1") {
    if(!as.character(attr(x, "explanatory")) %in% names(x)) {
      stop(paste0("The explanatory variable `", attr(x, "explanatory"), "` cannot be found in this dataframe."))
    }
    if(identical(as.character(attr(x, "response")), as.character(attr(x, "explanatory")))) {

    }


  attr(x, "success") <- success

  if (!all(
    as.character(
      c(attr(x, "response"),
        attr(x,"explanatory")
        )
    ) %in% names(x)
  )) stop("The columns you specified could not be found.")
  # TODO: coerce char to factor

  x <- as_tibble(x) %>%
    select(one_of(c(
      as.character((attr(x, "response"))),
      as.character(attr(x, "explanatory"))
    )))

  # add "infer" class
  class(x) <- append("infer", class(x))

  return(x)
}
