#' Specify a null hypothesis
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param null the null hypothesis
#' @param ... arguments passed to downstream functions
#' @importFrom dplyr as.tbl
#' @export
#' @examples 
#' if (require(dplyr)) {
#' 
#' # One binary variable
#'   mtcars %>%
#'     select(am) %>%
#'     hypothesize(null = "p = 25") %>%
#'     generate(reps = 100) %>%
#'     calculate(stat = "prop")
#'     
#' # Permutation test
#'   mtcars %>%
#'     select(mpg, cyl) %>%
#'     hypothesize(null = "rho = 0") %>%
#'     generate(reps = 100, type = "permute") %>%
#'     calculate(stat = "cor")
#' }
#' 
#' # Compare with
#' if (require(dplyr) && require(broom)) {
#'   cars <- mtcars %>%
#'     summarize(N = n(), num_manual = sum(am))
#'   with(cars, prop.test(num_manual, N, correct = FALSE)) %>%
#'     tidy()  
#' }

hypothesize <- function(x, null, ...) {
  attr(x, "null") <- null
  return(as.tbl(x))
}

#' Generate resamples
#' @param x a data frame that can be coerced into a \code{\link[dplyr]{tbl_df}}
#' @param reps the number of resamples to generate
#' @param type currently either \code{bootstrap} or \code{permute}
#' @param ... currently ignored
#' @importFrom dplyr group_by
#' @export

generate <- function(x, reps = 1, type = "bootstrap", ...) {
  if (type == "bootstrap") {
    return(bootstrap(x, reps, ...))
  }
  if (type == "permute") {
    return(permute(x, reps, ...))
  }
  x
}

bootstrap <- function(x, reps = 1, ...) {
  rep_sample_n(x, size = nrow(x), replace = TRUE, reps = reps)
}

#' @importFrom dplyr bind_rows mutate_ group_by

permute <- function(x, reps = 1, ...) {
  df_out <- replicate(reps, permute_once(x), simplify = FALSE) %>%
    dplyr::bind_rows() %>%
    dplyr::mutate_(replicate = ~rep(1:reps, each = nrow(x))) %>%
    dplyr::group_by(replicate)
  attr(df_out, "null") <- attr(x, "null")
  return(df_out)
}

permute_once <- function(x, ...) {
  dots <- list(...)
  
  if(attr(x, "null") == "equal means"){
    ## need to look for name of variable to permute...ugh
    ## by default, use the first column
    # y <- x[, 1]
    ## Hopefully this fixes that
    num_cols <- sapply(x, is.numeric)
    num_name <- names(num_cols[num_cols == TRUE])
    y <- x[[num_name]]
    
    y_prime <- y[ sample.int(length(y)) ]
    x[[num_name]] <- y_prime
    return(x)
  }
 
  if(attr(x, "null") == "independence"){
    ## by default, permute the first column of the two selected
    # Since dealing with tibble potentially, we need to force a
    # vector here
    y <- x[[1]]
    
    y_prime <- y[ sample.int(length(y)) ]
    x[[1]] <- y_prime
    return(x)
  }  
   
}


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

#' Calculate a p-value
#' @param x the output from \code{\link{calculate}}
#' @param ... currently ignored
#' @export

pvalue <- function(x, ...) {
  x
}

# Modified oilabs::rep_sample_n() with attr added
rep_sample_n <- function(tbl, size, replace = FALSE, reps = 1) {
 # attr(tbl, "ci") <- TRUE
  n <- nrow(tbl)
  i <- unlist(replicate(reps, sample.int(n, size, replace = replace), 
                        simplify = FALSE))
  rep_tbl <- cbind(replicate = rep(1:reps, rep(size, reps)), 
                   tbl[i, ])
  rep_tbl <- as_tibble(rep_tbl)
  names(rep_tbl)[-1] <- names(tbl)
#  attr(rep_tbl, "ci") <- attr(tbl, "ci")
  dplyr::group_by(rep_tbl, replicate)
}