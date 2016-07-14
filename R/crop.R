#' crop
#'
#' crop removes all columns and rows that are completely missing data. Note this is a cleaner, simpler version of dropAllNA().
#'
#' @param data data.frame to be cropped
#'
#' @return cropped data.frame
#' @export
#'
#' @examples
#' ## Example needs to be added
crop <- function(data) {
    dataLogical <- is.na(data)
    reducedDat <- data[rowMeans(dataLogical)!=1 ,colMeans(dataLogical)!=1]
    return(reducedDat)
}


