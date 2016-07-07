#' t_
#'
#' t_ is an expansion of the t() function, in which, if x is a data.frame(), it is transposed, and the first column of the output data.frame is the column names from the input data.frame.
#'
#' @param x matrix, vector, or data.frame or other class usable by t().
#'
#' @return a transposed object
#' @export
#'
#' @examples
#' #example_df <- data.frame(x1 = rnorm(100),
#' #                        x2 = rnorm(100),
#' #                        x3 = rnorm(100),
#' #                         x4 = rnorm(100),
#' #                         x5 = rnorm(100))
#' #t(rnorm(100))
#' #t_(rnorm(100))
#' #t(example_df)
#' #t_(example_df)
t_ <- function(x) {
  check <- is.data.frame(x)
  if(!check) {
    return(t(x))
  } else {
  tx <- t(x)
  colnames <- names(x)
  dat <- data.frame(colnames,tx)
  return(dat)
  }
}
