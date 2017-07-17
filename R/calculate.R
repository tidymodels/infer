#' Calculate summary statistics
#' @param x the output from \code{\link{hypothesize}} or \code{\link{generate}}
#' @param stat a string giving the type of the statistic to calculate. Current options include "mean", "prop", "diff in means", "diff in props", "chisq", and "F".
#' or an equation in quotes
#' @param ... currently ignored
#' @importFrom dplyr %>% group_by summarize
#' @importFrom rlang !! sym quo enquo
#' @export
#' @examples
#'
#' # Permutation test for two binary variables
#' if (require(dplyr)) {
#'   diffs <- mtcars %>%
#'     mutate(am = factor(am), vs = factor(vs)) %>%
#'     select(am, vs) %>%
#'     hypothesize(null = "independence") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "diff in props")
#'   test_stat <- mtcars %>%
#'     group_by(vs) %>%
#'     summarize(N = n(), manuals = sum(am)) %>%
#'     mutate(prop = manuals / N) %>%
#'     summarize(diff_prop = diff(prop))
#'   if (require(ggplot2)) {
#'     ggplot(data = diffs, aes(x = diffprop)) +
#'       geom_density() +
#'       geom_vline(xintercept = 0, linetype = 3) +
#'       geom_vline(data = test_stat, aes(xintercept = diff_prop), color = "red")
#'   }
#' }

calculate <- function(x, stat, ...) {

  if (stat == "mean") {
    col <- setdiff(names(x), "replicate")
    df_out <- x %>%
      dplyr::summarize(stat = mean(!!sym(col)))
  }

  if (stat == "prop") {
    col <- sym(setdiff(names(x), "replicate"))
    success <- quo(get_par_levels(x)[1])
    df_out <- x %>%
      dplyr::summarize(stat = mean((!! col) == eval_tidy(success)))
  }

  if (stat == "diff in means") {
    df_out <- x %>%
      dplyr::group_by_("replicate", attr(x, "explanatory")) %>%
      dplyr::summarize(xbar = mean(!! attr(x, "response"))) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(stat = diff(xbar))
  }

  if (stat == "diff in props") {
    df_out <- x %>%
      dplyr::group_by(replicate, attr(x, "explanatory")) %>%
      dplyr::summarize(prop = mean((!! attr(x, "response")) == levels(!! attr(x, "response"))[1])) %>%
      dplyr::summarize(stat = diff(prop))
  }

  if (stat == "Chisq") {
    ## The following could stand to be cleaned up
    n   <- attr(x, "biggest_group_size")

    if (is.null(attr(x, "explanatory"))) {
      expected <- n * attr(x, "params")
      df_out <- x %>%
        dplyr::summarize(stat = sum((table(!! attr(x, "response")) - expected)^2 / expected))
    } else {
      obs_tab <- x %>%
        filter(replicate == 1) %>%
        ungroup() %>%
        select(!! attr(x, "response"), !! attr(x, "explanatory")) %>%
        table()
      expected <- outer(rowSums(obs_tab), colSums(obs_tab)) / n
      df_out <- x %>%
        dplyr::summarize(stat = sum((table(!! attr(x, "response"), !! attr(x, "explanatory"))
                                     - expected)^2 / expected))

    }
  }

  if (stat == "F") {
    df_out <- x %>%
      dplyr::summarize(stat = anova(lm(!! attr(x, "response") ~ !! attr(x, "explanatory")))$`F value`[1])
  }

  if (stat == "slope") {
    df_out <- x %>%
      dplyr::summarize(stat = coef(lm(!! attr(x, "response") ~ !! attr(x, "explanatory")))[2])
  }

  return(df_out)
}
