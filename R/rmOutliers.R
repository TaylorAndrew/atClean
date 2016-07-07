#' rmOutliers
#'
#' rmOutliers converts outleirs (as defined as being some x number of standard deviations from the mean) to either NA, mean +/- x standard deviations, or the mean
#'
#' @param x Vector of continuous values
#' @param sdCut Number of standard deviations to use as a cut-point for finding outliers
#' @param method Method for outlier handling. 'remove' sets outliers to NA. 'limits' will set outliers to be equal to the mean +/- the standard deviation set. 'average' sets outliers to be equal to the average.
#'
#' @return A numeric vector with no outliers
#' @export
#'
#' @examples
#' #example_vector <- c(100, rnorm(19, 5, 2))
#' #rmOutliers(example_vector, 3, method = "remove")
#' #rmOutliers(example_vector, 3, method = "limits")
#' #rmOutliers(example_vector, 3, method = "average")
rmOutliers <- function(x, sdCut = 3, method = "remove") {
  if (!method %in% c("remove","limits","average")) {
    print("method not in c('remove', 'limits', 'average'), defaulting to 'remove'")
    method = "remove"
  }
  x <- as.numeric(as.character(x))

  mnX <- mean(x, na.rm = T)
  sdX <- sd(x, na.rm = T)
  upperX <- mnX + (sdCut * sdX)
  lowerX <- mnX - (sdCut * sdX)

  upperIndx <- x > upperX
  lowerIndx <- x < lowerX

  if (method == "remove") {
    x[upperIndx] <- NA
    x[lowerIndx] <- NA
  } else if (method == "limits") {
    x[upperIndx] <- upperX
    x[lowerIndx] <- lowerX
  } else if (method == "average") {
    x[upperIndx] <- NA
    x[lowerIndx] <- NA
    newMnX <- mean(x, na.rm = T)
    x[upperIndx] <- newMnX
    x[lowerIndx] <- newMnX

  }
  return(x)
}
