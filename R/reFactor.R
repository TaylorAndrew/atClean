#' reFactor
#'
#' reFactor will refactor a categorical variable such that the order of the levels have changed (e.g change what the reference group will be)
#'
#' @param x Vector to be refactored
#' @param order_index Index of the current factor levels in which to refactor with
#' @param order_levels Actual Vector levels to use to refactor with
#'
#' @return Refactored vector
#' @export
#'
#' @examples
#' #v <- sample(letters[1:3], 10, replace = T)
#' #factor(v)
#' #Reverse order
#' #reFactor(v)
#' #refactor based on original factor index
#' #reFactor(v, order_index = c(2, 1, 3))
#' #refactor based on actual levels
#' #reFactor(v, order_levels = c("b", "a", "c"))
reFactor <- function(x,
                     order_index = NULL,
                     order_levels = NULL) {
  len <- length(levels(factor(x)))
  ord <- len:1
  if (!is.null(order_index)) {
    if (length(order_index) != length(unique(order_index))) {
      return(
        print(
          "At least one index location is duplicated in order_index. Please check and include every index location of x exactly once."
        )
      )
    } else if (length(order_index) != length(ord)) {
      return(
        print(
          "The length of order_index does not match the number of levels of x. Please check and fix order_index."
        )
      )
    } else{
      ord <- order_index
    }
  } else if (!is.null(order_levels)) {
    if (length(order_levels) != length(unique(order_levels))) {
      return(
        print(
          "At least one level is duplicated in order_levels or less/more than the actual number of levels were provided. Please check and include every level of x exactly once and no other levels are included."
        )
      )
    }
    check1 <- order_levels %in% levels(factor(x))
    check2 <- levels(factor(x)) %in% order_levels
    if (min(as.numeric(check1))  == 0 |
        min(as.numeric(check2) == 0)) {
      return(
        print(
          "There is an issue with the levels provided. They do not match the actual levels of x. Please check."
        )
      )
    } else {
      ord <- c()
      for (i in 1:length(order))
        ord[i] <- which(levels(factor(x)) == order_levels[i])
    }
  }
x = factor(x, levels(factor(x))[ord])
return(x)
}
