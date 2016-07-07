#' applyLabels
#'
#' applyLabels will take either a single variable or a list of variables and apply a set of labels.
#'
#' @param data Input data.frame that contains the to-be labeled variable(s)
#' @param varlist Vector of to-be labled variable names
#' @param labels Vector of labels to apply to the selected variable(s)
#' @param fullLevels if the length of labels does not match the number of levels found in data, this will provide the 'theoretical' full levels that 'labels' was based on
#'
#' @return A data.frame with labels added to the variables listed in varlist
#' @export
#'
#' @examples
#' #fakeData <- data.frame(x = sample(c(0, 1), 10, replace=T))
#' #applyLabels(data = fakeData, varlist = c("x"), labels = c("No", "Yes"))
applyLabels <- function(data, varlist, labels, fullLevels = NULL) {
  data <- as.data.frame(data)
  i = 0
  orderVars <- names(data)
  applyto <- function(var) {
    i <<- i + 1
    data[, var] <- factor(data[, var])
    levels <- levels(data[, var])
    if (length(levels) != length(labels) &
        is.null(fullLevels) == TRUE) {
      print(
        paste0(
          "Number of Levels of ",
          varlist[i],
          ": (",
          length(levels),
          ": ",
          paste0(levels, collapse = ", "),
          ") ",
          "is not the same as the number of labels: (",
          length(labels),
          ": ",
          paste0(labels, collapse = ", "),
          ")"
        )
      )
      j <- as.data.frame(data[, var])
      return(j)
    }
    if (!is.null(fullLevels))
      levels <- fullLevels
    if (length(levels) == length(labels)) {
      data[, var] <- factor(data[, var],
                            levels = levels,
                            labels = labels)
      j <- as.data.frame(data[, var])
      return(j)
    }
  }
  dat <- do.call(cbind, lapply(varlist, applyto))
  newvars <- length(dat[1, ])
  if (newvars >= 1) {
    names(dat) <- varlist
    datother <- data[, !names(data) %in% names(dat)]
    finaldata <- data.frame(datother, dat)
    names(finaldata) <-
      c(names(data)[!names(data) %in% names(dat)], varlist)
    finaldata <- finaldata[, orderVars]
  }
  if (newvars == 0L) {
    finaldata <- data
  }
  return(finaldata)
}
