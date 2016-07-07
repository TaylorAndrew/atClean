#' logical_join
#'
#' logical_join provides booliean variable appended onto data1, indicating which rows have matching observations in data2
#'
#' @param data1 data.frame to join as reference dataset
#' @param data2 data.frame to look for potential matches
#' @param by variable or variables to use for joining data1 and data2
#'
#' @return data1 is returned with a boolian variable 'inData2' appended.
#' @export
#'
#' @examples
#' #df1 <- data.frame(id = 1:10, x = rnorm(10))
#' #df2 <- data.frame(id = sample(c(1:10), 5, replace = F), y = rnorm(10))
#' #logical_join(df1, df2)
#' #df1 %>% logical_join(df2)
#' #df1 %>% logical_join(df2, by = 'id')
logical_join <- function(data1, data2, by = NULL) {
  temp1 <- semi_join(data1, data2, by = if (is.null(by)) {
    NULL
  } else {
    by
  })
  temp2 <- rbind(temp1,data1)
  return(data.frame(data1, inData2 = duplicated(temp2)[-c(1:length(temp1[,1]))]))
}
