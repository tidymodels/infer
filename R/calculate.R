#' Calculate summary statistics
#' @param x the output from \code{\link{hypothesize}} or \code{\link{generate}}
#' @param stat a string giving the type of the statistic to calculate. Current 
#' options include "mean", "median", "sd", "prop", "diff in means", 
#' "diff in medians", "diff in props", "Chisq", "F", and "slope".
#' @param success a string specifying which level of the response variable should be
#' used as a success in bootstrapping a one proportion confidence interval
#' @param ... to pass options like \code{na.rm = TRUE} into functions like mean, sd, etc.
#' @importFrom dplyr group_by summarize
#' @importFrom rlang !! sym quo enquo eval_tidy
#' @export
#' @examples
#' # Permutation test for two binary variables
#' if(require(dplyr)) {
#'   mtcars %>%
#'     mutate(am = factor(am), vs = factor(vs)) %>%
#'     specify(am ~ vs) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "diff in props") %>%
#'     visualize()
#' }
#' 
#' # Confidence interval using bootstrapping for one proportion
#' if(require(dplyr)) {
#'     mtcars %>%
#'         mutate(am = factor(am)) %>%
#'         specify(response = am) %>%
#'         generate(reps = 100, type = "bootstrap") %>%
#'         calculate(stat = "prop", success = "1")
#' }

calculate <- function(x, stat, success = NULL, ...) {

  # TODO: Check to see if dplyr::group_by(replicate) is needed since
  # generate() does a grouping of replicate
  
  if (stat == "mean") {
    col <- setdiff(names(x), "replicate")
    df_out <- x %>%
      dplyr::group_by(replicate) %>% 
      dplyr::summarize(stat = mean(!!sym(col), ...))
  }
  
  if (stat == "median") {
    col <- setdiff(names(x), "replicate")
    df_out <- x %>%
      dplyr::group_by(replicate) %>% 
      dplyr::summarize(stat = stats::median(!!sym(col), ...))
  }

    if (stat == "sd") {
    col <- setdiff(names(x), "replicate")
    df_out <- x %>%
      dplyr::group_by(replicate) %>% 
      dplyr::summarize(stat = stats::sd(!!sym(col), ...))
  }

    if (stat == "sd") {
    col <- setdiff(names(x), "replicate")
    df_out <- x %>%
      dplyr::group_by(replicate) %>% 
      dplyr::summarize(stat = stats::sd(!!sym(col)))
  }

  if (stat == "prop") {
    col <- attr(x, "response")
    if(is.null(success))
      success <- quo(get_par_levels(x)[1])
    df_out <- x %>%
     # dplyr::summarize(stat = mean(!! col == rlang::eval_tidy(success))) # This doesn't appear to be working
      dplyr::group_by(replicate) %>% 
      # The following works but not sure why when looking at the diff in means code?
      dplyr::summarize(stat = mean(rlang::eval_tidy(col) == rlang::eval_tidy(success), ...))
  }

  if (stat == "diff in means") {
    df_out <- x %>%
      dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
      dplyr::summarize(xbar = mean(!!attr(x, "response"), ...)) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = diff(xbar))
  }

  if (stat == "diff in medians") {
    df_out <- x %>%
      dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
      dplyr::summarize(xtilde = stats::median(!!attr(x, "response"), ...)) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = diff(xtilde))
  }
  
  if (stat == "diff in props") {
    df_out <- x %>%
      dplyr::group_by(replicate, !!attr(x, "explanatory")) %>%
      dplyr::summarize(prop = mean((!!attr(x, "response")) == levels(!!attr(x, "response"))[1], ...)) %>%
      dplyr::summarize(stat = diff(prop))
  }

  if (stat == "Chisq") {
    ## The following could stand to be cleaned up
    n   <- attr(x, "biggest_group_size")

    if (is.null(attr(x, "explanatory"))) {
      expected <- n * attr(x, "params")
      df_out <- x %>%
        dplyr::summarize(stat = sum((table(!!attr(x, "response")) - expected)^2 / expected, ...))
    } else {
      obs_tab <- x %>%
        dplyr::filter(replicate == 1) %>%
        dplyr::ungroup() %>%
        dplyr::select(!!attr(x, "response"), !!attr(x, "explanatory")) %>%
        table()
      expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
      df_out <- x %>%
        dplyr::summarize(stat = sum((table(!!attr(x, "response"), !!attr(x, "explanatory"))
                                     - expected)^2 / expected, ...))

    }
  }

  if (stat == "F") {
    df_out <- x %>%
      dplyr::summarize(stat = stats::anova(
          stats::lm(!! attr(x, "response") ~ !! attr(x, "explanatory"))
        )$`F value`[1])
  }

  if (stat == "slope") {
    df_out <- x %>%
      dplyr::summarize(stat = stats::coef(stats::lm(!! attr(x, "response") ~ !! attr(x, "explanatory")))[2])
  }

  return(df_out)
}
