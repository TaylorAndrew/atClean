#' substrRight
#'
#' substrRight returns a substring of a string or vector of strings of length less than or equal to a given `n`, starting with the right-most character.
#'
#' @param x Character string or vector of character strings
#' @param n Number of characters to exract from strings starting with the right-most character.
#'
#' @return A string or vector of strings of length less than or equal to n, consisting of characters on the right side of the strings.
#' @export
#'
#' @examples
#' #x = "OneString"
#' #x2 = c("ThisIsOne", "ThisIsTwo", "This is three", "And this4")
#' #Regular substr() usage
#' #substr(x, start = 1, stop = 3)
#' #substr(x2, start = 1, stop = 3)
#' #substrRight usage
#' #substrRight(x, n = 3)
#' #substrRight(x2, n = 3)
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}
