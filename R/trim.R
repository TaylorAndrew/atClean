#' trim
#'
#' trim removes whitespace from the left and right side. Depricated with R v3.0.0 (trimws())
#'
#' @param x string or vector of strings
#' @param side Which side should be trimmed: 'both', 'left', or 'right'
#'
#' @return string or vector of strings
#' @export
#'
#' @examples
#' #x = "bill "
#' #x2 = c("bill ", " bill", " bill ")
#' #trim(x)
#' #trim(x2)
#' #trim(x2, "left")
#' #trim(x2, "right")
trim <- function (x, side = "both") {
  if(side=="both") {return(gsub("^\\s+|\\s+$", "", x))}
  if(side=="left") {return(gsub("^\\s+", "", x))}
  if(side=="right") {return(gsub("\\s+$", "", x))}
}
