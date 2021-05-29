#' @importFrom generics fit
#' @export
generics::fit

#' Fit linear models to infer objects
#'
#' @description
#' 
#' Given the output of an infer core function, this function will fit
#' a model according to the formula and data supplied earlier in the
#' pipeline. If passed the output of [specify()] or [hypothesize()], the
#' function will fit one model. If passed the output of [generate()],
#' it will fit a model to each data resample, denoted in the `replicate` column.
#'
#' Learn more in `vignette("infer")`.
#'
#' @param object Output from an infer function---likely [generate()] or 
#' [hypothesize()]---which specifies the formula and data to fit a model to.
#' @param engine A string specifying which package or system will be used to 
#'   fit the model. See [parsnip::linear_reg()] for information on possible 
#'   values. Defaults to the base [stats::lm()] function.
#' @param ... Any optional arguments to pass along to the chosen computational 
#'   engine. See [parsnip::linear_reg()] for more information.
#'
#' @return 
#' A tibble. If the input had not been passed to [generate()], the tibble
#' will contain `term` and `stat` columns. If it had been, there will also
#' be a `replicate` column.
#' 
#' \itemize{
#'   \item `replicate`: Only supplied if the input object had been previously
#'     passed to [generate()]. A number corresponding to which resample of the
#'     original data set the model was fitted to.
#'   \item `term`: The explanatory variable (or intercept) in question.
#'   \item `estimate`: The model coefficient for the given resample (`replicate`) and 
#'     explanatory variable (`term`).
#'   \item `stat`: The value of a t-statistic under the null hypothesis that 
#'     the regression estimate is non-zero.
#' }
#'
#' @examples
#' 
#' # More in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }
#' 
#' @method fit infer
#' @export fit.infer
#' @export
fit.infer <- function(object, engine = "lm", ...) {
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
          engine = engine, 
          ...))
      ) %>%
      dplyr::select(replicate, model) %>%
      tidyr::unnest(model)
  } else {
    x <- fit_linear_model(object, formula, engine = engine, ...)
  }
  
  copy_attrs(x, object)
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

fit_linear_model <- function(object, formula, engine, ...) {
  parsnip::linear_reg(...) %>% 
    parsnip::set_engine(engine) %>%
    fit(
      formula = formula,
      data = tibble::as_tibble(object)
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
