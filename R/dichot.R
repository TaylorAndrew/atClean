#' dichot
#'
#' dichot will take a variable and dichotomize it into 0, 1 (and NA for any unspecified levels)
#'
#' @param x input data
#' @param group1 vector of levels to be set to 0
#' @param group2 vector of levels to be set to 1
#'
#' @return output vector of 0, 1, NA
#' @export
#'
#' @examples
#' #fakeData <- data.frame(x = sample(c(1, 2, 3, 4, 5), 20, replace = T))
#' #dichot(x = fakeData$x, group1 = c(1, 2), group2 = c(3, 4))
dichot <- function(x, group1, group2) {
  newvar <- ifelse(x %in% group1, 0,
                   ifelse(data[,oldvar] %in% group2,1,NA))
  return(newvar)
}
