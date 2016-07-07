#' dummyCode
#'
#' dummyCode will create a dummy coded set of variables
#'
#' @param x input data
#' @param convertNA TRUE/FALSE: Should NA be converted to a unique level
#'
#' @return A data.frame with dummy coded variables
#' @export
#'
#' @examples
#' #x <- sample(c(1,2,3,4,5), 20, replace = T)
#' #dummyCode(x = x)
dummyCode <- function(x,
                      convertNA = TRUE) {
  if (convertNA == TRUE) {
    new_var <- ifelse(is.na(x), "unknownNA", x)
  } else {
    new_var <- x
  }
  IncludeNewLevel <- "unknownNA" %in% levels(factor(new_var))
  lev <- levels(factor(new_var))
  makedum <- function(i) {
    single <- ifelse(new_var == lev[i], 1, 0)
    single <- ifelse(is.na(new_var), NA, single)
    assign(lev[i], single)
  }
  dumcodes <- do.call(cbind, lapply(c(1:length(lev)), makedum))
  colnames(dumcodes) <- lev
  dumcodes <- as.data.frame(dumcodes)
  return(dumcodes)
}
