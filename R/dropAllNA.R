#' dropAllNA
#'
#' dropAllNA  will drop all columns, rows, or both, where all entries in the data.frame are NA
#'
#' @param data data.frame containing data
#' @param axis which axis to remove from: 'row' removes all rows where all columns have missing data. 'col' removes all columns where all rows are NA. 'all' does both.
#'
#' @return data frame with the rows/columns missing
#' @export
#'
#' @examples
#' #df <- data.frame(a = c(1, 2, NA, 4),
#' #                 b = c(5, 6, NA, 7),
#' #                 c = c(NA, NA, NA, NA),
#' #                 d = c(8, 9, NA, 10))
#' #dropAllNA(df) #default is 'all'
#' #dropAllNA(df, 'all') #remove rows and cols with missing
#' #dropAllNA(df, 'col') #remove cols with missing
#' #dropAllNA(df, 'row') #remove rows with missing
dropAllNA <- function(data, axis = c('all', 'col', 'row')) {
  axis = axis[1]
  removeEmptyVars <- function(data) {
  smallData <- data[, colSums(!is.na(data)) != 0]
  return(smallData)
  }
  removeEmptyRows <- function(data) {
  smallData <- data[rowSums(!is.na(data)) != 0, ]
  return(smallData)
  }
  if(axis == 'col') return(removeEmptyVars(data))
  if(axis == 'row') return(removeEmptyRows(data))
  if(axis == 'all') return(removeEmptyVars(removeEmptyRows(data)))
}
