#' Declare a null hypothesis
#'
#' @param x A data frame that can be coerced into a [tibble][tibble::tibble].
#' @param null The null hypothesis. Options include `"independence"` and
#'   `"point"`.
#' @param p The estimated proportion of successes as a number between zero and
#' one. To be used with point null hypotheses when the specified response
#' variable is categorical.
#' @param mu The estimated mean as a number. To be used with point null
#' hypotheses when the specified response variable is continuous.
#' @param med The estimated median as a number. To be used with point null
#' hypotheses when the specified response variable is continuous.
#' @param sigma The estimated standard deviation as a number. To be used with
#' point null hypotheses.
#'
#' @return A tibble containing the response (and explanatory, if specified)
#'   variable data with parameter information stored as well.
#'
#' @examples
#' # Permutation test similar to ANOVA
#' mtcars %>%
#'   dplyr::mutate(cyl = factor(cyl)) %>%
#'   specify(mpg ~ cyl) %>%
#'   hypothesize(null = "independence") %>%
#'   generate(reps = 100, type = "permute") %>%
#'   calculate(stat = "F")
#'
#' @importFrom purrr compact
#' @export
hypothesize <- function(x, null, p = NULL, mu = NULL, med = NULL, sigma = NULL) {
  hypothesize_checks(x, null)

  # Custom logic, because using match.arg() would give a default value when
  # the user didn't specify anything.
  null <- match_null_hypothesis(null)
  attr(x, "null") <- null

  dots <- compact(list(p = p, mu = mu, med = med, sigma = sigma))

  switch(
    null,
    independence =  {
      params <- sanitize_hypothesis_params_independence(dots)
      attr(x, "type") <- "permute"
    },
    point = {
      params <- sanitize_hypothesis_params_point(dots, x)
      attr(x, "params") <- unlist(params)

      if (!is.null(params$p)) {
        # simulate instead of bootstrap based on the value of `p` provided
        attr(x, "type") <- "simulate"
      } else {
        # Check one proportion test set up correctly
        if (is.factor(response_variable(x))) {
          stop_glue(
            'Testing one categorical variable requires `p` to be used as a ',
            'parameter.'
          )
        }
        attr(x, "type") <- "bootstrap"
      }
    }
  )
  append_infer_class(tibble::as_tibble(x))
}
