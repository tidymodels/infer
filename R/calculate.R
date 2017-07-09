#' Calculate summary statistics
#' @param x the output from \code{\link{hypothesize}} or \code{\link{generate}}
#' @param stat a string giving the type of the statistic to calculate. Current options include "mean", "prop", "diff in means", "diff in props", "chisq", and "F".
#' or an equation in quotes
#' @param ... currently ignored
#' @importFrom dplyr %>% group_by group_by_ summarize_ summarize
#' @importFrom lazyeval interp
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
      #dplyr::group_by(replicate) %>%
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
    # Assume the first column is to be permuted and
    # the second column are the groups
    # Assumes the variables are factors and NOT chars here!
    permute_col <- names(x)[1]
    group_col <- names(x)[2]

    df_out <- x %>%
      dplyr::group_by_("replicate", .dots = group_col) %>%
      dplyr::summarize_(N = ~n(),
                        prop = lazyeval::interp(~mean(var == levels(var)[1]),
                                                var = as.name(permute_col))) %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize_(diffprop = ~diff(prop))
  }

  if (stat == "Chisq") {

  }


  if (stat == "F") {

  }

  return(df_out)
}
