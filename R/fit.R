#' @importFrom generics fit
#' @export
generics::fit

#' Fit linear models to infer objects
#'
#' @description
#' 
#' Given the output of an infer core function, this function will fit
#' a model using [stats::glm()] according to the formula and data supplied 
#' earlier in the pipeline. If passed the output of [specify()] or 
#' [hypothesize()], the function will fit one model. If passed the output 
#' of [generate()], it will fit a model to each data resample, denoted in 
#' the `replicate` column.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param object Output from an infer function---likely [generate()] or 
#' [hypothesize()]---which specifies the formula and data to fit a model to.
#' @param ... Any optional arguments to pass along to the chosen computational 
#'   engine. See [parsnip::linear_reg()] for more information.
#'
#' @return A [tibble][tibble::tibble] containing the following columns:
#' 
#' \itemize{
#'   \item `replicate`: Only supplied if the input object had been previously
#'     passed to [generate()]. A number corresponding to which resample of the
#'     original data set the model was fitted to.
#'   \item `term`: The explanatory variable (or intercept) in question.
#'   \item `estimate`: The model coefficient for the given resample (`replicate`) and 
#'     explanatory variable (`term`).
#' }
#'
#' @examples
#' # fit a linear model predicting number of hours worked per
#' # week using respondent age and degree status.
#' observed_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   hypothesize(null = "independence") %>%
#'   fit()
#' 
#' observed_fit
#' 
#' # fit 100 models to resamples of the gss dataset, where the response 
#' # `hours` is permuted in each. note that this code is the same as 
#' # the above except for the addition of the `generate` step.
#' null_fits <- gss %>%
#'   specify(hours ~ age + college) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   fit()
#' 
#' null_fits
#' 
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }  
#' 
#' @method fit infer
#' @export fit.infer
#' @export
fit.infer <- function(object, ...) {
  message_on_excessive_null(object, fn = "fit")
  
  # Extract the formula if it was supplied to specify, otherwise
  # construct it out of the explanatory and response arguments
  formula <- get_formula(object)
  
  if (is_generated(object)) {
    x <- object %>%
      tidyr::nest(data = -replicate) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = list(fit_linear_model(
          object = data, 
          formula = formula,
          ...))
      ) %>%
      dplyr::select(replicate, model) %>%
      tidyr::unnest(model)
  } else {
    x <- fit_linear_model(object, formula, ...)
  }
  
  x <- copy_attrs(x, object)
  attr(x, "fitted") <- TRUE
  
  x
}

get_formula <- function(x) {
  if (has_attr(x, "formula")) {
    return(attr(x, "formula"))
  } else {
    exp <- paste0(explanatory_name(x), collapse = " + ")
    
    as.formula(
      glue_null(
        '{response_name(x)} ~ 
         {if (exp == "") NULL else exp}'
      )
    )
  }
}

fit_linear_model <- function(object, formula, ...) {
  stats::glm(
      formula = formula,
      data = object,
      ...
    ) %>%
    generics::tidy() %>%
    dplyr::select(
      .,
      term, 
      estimate
    ) %>%
    dplyr::mutate(
      term = dplyr::case_when(
        term == "(Intercept)" ~ "intercept", 
        TRUE ~ term
      )
    )
}
