#' row_lvcf
#'
#' @param data data.frame containing data
#' @param cols numbers to carry forward (in the order to carry forward)
#'
#' @return data.frame with last values carried forward
#' @export
#'
#' @examples
#' # Needs an example
row_lvcf <- function(data, cols) {
  #initialize at the second column
  i = cols[2]
  #per column
  onecol <- function(data, i) {
    #if data is missing and the column prior is non-missing, fill it in
    data[,i] <- ifelse(is.na(data[, i]) & !is.na(data[, i-1]),
                  data[, i-1], data[, i])
    #index to the next column
    i <- i + 1
    #Recursively iterate through the chunk until finished
    if(i > cols[length(cols)]) {
      return(data) } else {
        onecol(data, i)
      }
  }
  #return the lvcf dataset
  return(onecol(data, i))
}