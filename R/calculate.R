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
    num_cols <- sapply(x, is.numeric)
    non_num_name <- names(num_cols[num_cols != TRUE])
    col <- setdiff(names(x), "replicate")
    col <- setdiff(col, non_num_name)
    df_out <- x %>%
      dplyr::group_by_("replicate", .dots = non_num_name) %>%
      dplyr::summarize_(N = ~n(),
                        mean = lazyeval::interp(~mean(var), var = as.name(col))) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize(diffmean = diff(mean))
  }

  if (stat == "diff in props") {
    permute_col <- attr(x, "response")
    group_col <- attr(x, "explanatory")

    df_out <- x %>%
      dplyr::group_by(replicate, !! group_col) %>%
      dplyr::summarize(prop = mean((!! permute_col) == levels(!! permute_col)[1])) %>%
      dplyr::summarize(stat = diff(prop))
  }

  if (stat == "slope") {
    model <- as.formula(paste0(attr(x, "response"), "~", attr(x, "explanatory")))
    df_out <- x %>%
      dplyr::summarize(stat = coef(lm(eval_tidy(model)), data = )[2])
  }

  if (stat == "Chisq") {
    col <- sym(setdiff(names(x), "replicate"))
    n   <- attr(x, "biggest_group_size")
    expected <- n * attr(x, "params")
    df_out <- x %>%
      dplyr::summarize(stat = sum((table(!! col) - expected)^2 / expected))
  }


  if (stat == "F") {

  }

  return(df_out)
}
