#' @importFrom generics fit
#' @details 
#' Read more about infer's [fit][fit.infer()] function [here][fit.infer()] or 
#' by running `?fit.infer` in your console.
#' 
#' @export
generics::fit


#' Fit linear models to infer objects
#' 
#' @description
#' Given the output of an infer core function, this function will fit
#' a model using [stats::glm()] according to the formula and data supplied 
#' earlier in the pipeline. If passed the output of [specify()] or 
#' [hypothesize()], the function will fit one model. If passed the output 
#' of [generate()], it will fit a model to each data resample, denoted in 
#' the `replicate` column.
#' 
#' infer provides a fit "method" for infer objects, which is a way of carrying 
#' out model fitting as applied to infer output. The "generic," imported from
#' the generics package and re-exported from this package, provides the
#' general form of `fit()` that points to infer's method when called on an
#' infer object. That generic is also documented here.
#'
#' Learn more in `vignette("infer")`.
#' 
#' @param object Output from an infer function---likely [generate()] or 
#' [specify()]---which specifies the formula and data to fit a model to.
#' @param mode One of "regression" or "classification"---the model type,
#' determined by the outcome variable. If a `family` argument is passed
#' via `...`, this argument will be ignored.
#' @param ... Any optional arguments to pass along to the model fitting
#' function. See [stats::glm()] for more information.
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
#' @details 
#' 
#' Randomization-based statistical inference with multiple explanatory 
#' variables requires careful consideration of the null hypothesis in question
#' and its implications for permutation procedures. Inference for partial
#' regression coefficients via the permutation method implemented in 
#' [generate()] for multiple explanatory variables, consistent with its meaning 
#' elsewhere in the package, is subject to additional distributional assumptions
#' beyond those required for one explanatory variable. Namely, the distribution
#' of the response variable must be similar to the distribution of the errors
#' under the null hypothesis' specification of a fixed effect of the explanatory 
#' variables. (This null hypothesis is reflected in the `cols` argument to 
#' [generate()]. By default, all of the explanatory variables are treated
#' as fixed.) A general rule of thumb here is, if there are large outliers
#' in the distributions of any of the explanatory variables, this distributional
#' assumption will not be satisfied; when the response variable is permuted,
#' the (presumably outlying) value of the response will no longer be paired
#' with the outlier in the explanatory variable, causing an outsize effect
#' on the resulting slope coefficient for that explanatory variable.
#' 
#' More sophisticated methods that are outside of the scope of this package
#' requiring fewer---or less strict---distributional assumptions 
#' exist. For an overview, see "Permutation tests for univariate or 
#' multivariate analysis of variance and regression" (Marti J. Anderson,
#' 2001), \doi{10.1139/cjfas-58-3-626}.
#' 
#' @examples
#' # fit a linear model predicting number of hours worked per
#' # week using respondent age and degree status.
#' observed_fit <- gss %>%
#'   specify(hours ~ age + college) %>%
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
#' # for logistic regression, set `mode = "classification"`
#' gss %>%
#'   specify(college ~ age + hours) %>%
#'   fit(mode = "classification")
#' 
#' # more in-depth explanation of how to use the infer package
#' \dontrun{
#' vignette("infer")
#' }  
#' 
#' @rdname fit.infer
#' @method fit infer
#' @export fit.infer
#' @export
fit.infer <- function(object, mode = "regression", ...) {
  message_on_excessive_null(object, fn = "fit")
  
  # Confirm that the mode, either supplied via the mode
  # argument or via `family` in ..., aligns with the outcome
  # variable. Return a processed version of the ellipses
  dots <- check_mode(object, mode, ...)

  # Relevel the response based on the success attribute
  # so that the reference level is reflected in the fit
  object <- relevel_response(object)
  
  # Extract the formula if it was supplied to specify, otherwise
  # construct it out of the explanatory and response arguments
  formula <- get_formula(object)
  
  if (is_generated(object)) {
    x <- object %>%
      tidyr::nest(data = -replicate) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        model = list(
          do.call(
            fit_linear_model,
            c(
              list(object, formula),
              dots
            )
          )
        )
      ) %>%
      dplyr::select(replicate, model) %>%
      tidyr::unnest(model)
  } else {
    x <- do.call(
      fit_linear_model,
      c(
        list(object, formula),
        dots
      )
    )
  }
  
  x <- copy_attrs(x, object)
  attr(x, "fitted") <- TRUE
  
  x
}

check_mode <- function(object, mode, ...) {
  response_type <- determine_variable_type(object, "response")
  
  if (response_type == "mult") {
    stop_glue(
      "infer does not support fitting models on categorical response variables ",
      "with more than two levels. Please see `multinom_reg()` from the ",
      "parsnip package."
    )
  }
  
  dots <- list(...)
  
  if ("family" %in% names(dots)) {
    return(dots)
  }
  
  if (!mode %in% c("regression", "classification")) {
    stop_glue(
      'The `mode` argument must be one of "regression" or "classification".'
    )
  }
  
  if ((response_type == "num" && mode == "classification") ||
      (response_type == "bin" && mode == "regression")) {
    stop_glue(
      'Use `mode = "{switch_mode(mode)}"` in `fit()` for a ',
      '{get_stat_type_desc(response_type)} response variable.'
    )
  }
  
  if (mode == "classification") {
    dots[["family"]] <- stats::binomial
  } else {
    dots[["family"]] <- stats::gaussian
  }
  
  dots
}

switch_mode <- function(mode) {
  if (mode == "regression") {
    return("classification")
  } else {
    return("regression")
  }
}

relevel_response <- function(x) {
  if (!is.null(attr(x, "success"))) {
    x[[response_name(x)]] <- 
      relevel(
        response_variable(x), 
        ref = attr(x, "success")
      )
  }

  
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
    broom::tidy() %>%
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
