#' Calculate summary statistics
#' @param x the output from \code{\link{hypothesize}} or \code{\link{generate}}
#' @param stat a string giving the type of the statistic to create, i.e. "diff in means", "diff in props", etc.
#' or an equation in quotes
#' @param ... currently ignored
#' @importFrom dplyr %>% group_by group_by_ summarize_ summarize
#' @importFrom lazyeval interp
#' @export

calculate <- function(x, stat, ...) {

  if(stat == "diff in means"){
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
    return(df_out)
  }

  if(stat == "diff in props"){
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
      dplyr::summarize(diffprop = diff(prop))
    return(df_out)
  }

  if(stat == "mean"){
    col <- setdiff(names(x), "replicate")
    x %>%
      dplyr::group_by(replicate) %>%
      dplyr::summarize_(mean = lazyeval::interp(~mean(var),
                                                var = as.name(col)))
  }

}
