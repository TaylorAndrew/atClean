#' normalize
#'
#' normalize transforms a numeric vector to percentile ranks
#'
#' @param x A numeric vector
#' @param method If method=1, standard normalize method used. If method=2, percentile ranks are provided.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' #x <- seq(1:10)
#' #x2 = runif(100, 1, 44)
#' #normalize(x, 1)
#' #normalize(x, 2)
#' #normalize(x2, 1)
#' #normalize(x2, 2)
normalize <- function(x, method=2) {
  if(method==1) return((x - min(x)) / (max(x) - min(x)))
  if(method==2) return((x) / (max(x)))
}
