#' multChoiceCondense
#'
#' multChoiceCondense takes a set of variables and condenses them down to a single variable
#'
#' @param data Input dataset
#' @param varList List of variables to condense
#' @param resp Response used across varList indicating that the person responded to that column. If NULL, then any non-NA response will be used as the indicator.
#' @param cellEntry Indicates what should be intered into the new variable. If 'number', then the column number will be used. If 'original', then the cell contents will be used. If 'column name', then the name of the column indicated as the response will be used.
#' @param multipleResponses Indicates what should be done if the person has responses across multiple columns. If 'first', then the first column (in the order listed in varList) with a valid response is used. If 'last', then the first column (in the order listed in varList) with a valid response is used. If 'combine', then all valid responses are combined.
#'
#' @return A vector with the collapsed data from the input data.
#' @export
#'
#' @examples
#'# missdata<-data.frame(id = 1:1000,                                              #
#'#                     C1 = sample(c(1,NA),1000,replace=TRUE,prob=c(.4,.6)),     #
#'#                     C2 = sample(c(1,NA),1000,replace=TRUE,prob=c(.4,.6)),     #
#'#                     C3 = sample(c(1,NA),1000,replace=TRUE,prob=c(.4,.6)),     #
#'#                     C4 = sample(c(1,NA),1000,replace=TRUE,prob=c(.4,.6)),     #
#'#                     C5 = sample(c(1,NA),1000,replace=TRUE,prob=c(.4,.6)))     #
#'# View(missdata)
#'# varList<-c("C1","C2","C3","C4","C5")                                             #
#'# multChoiceCondense(data = missdata, varList = varList, cellEntry = 'number', multipleResponses = 'first')
#'# multChoiceCondense(data = missdata, varList = varList, cellEntry = 'number', multipleResponses = 'combine')
#'# respdata <- data.frame(White = sample(c("Yes", "No"), 30, replace = T, prob = c(.2, .8)),
#'#                        Black = sample(c("Yes", "No"), 30, replace = T, prob = c(.2, .8)),
#'#                        Asian = sample(c("Yes", "No"), 30, replace = T, prob = c(.2, .8)))
#'# respdata$Other <- ifelse(respdata$White=="No" & respdata$Black=="No" & respdata$Asian=="No", "Yes", "No")
#'# View(respdata)
#'# varList2<-c("White", "Black", "Asian", "Other")
#'# multChoiceCondense(data = respdata, resp = 'Yes', varList = varList2, cellEntry = 'column name', multipleResponses = 'combine')
multChoiceCondense <-
  function(data,
           varList,
           resp = NULL,
           cellEntry = "number",
           multipleResponses = "first") {
    if (!cellEntry%in%c("number","original", "column name")) {
      return(print("cellEntry must be one of: 'number', 'original', 'column name'"))
    }
    if (!multipleResponses%in%c("first","last", "combine")) {
      return(print("multipleResponses must be one of: 'first', 'last', 'combine'"))
    }
    if (cellEntry == "number") {
      if (is.null(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (!is.na(dat[j,i]))
              if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {tempvar[j] = i
                }
              } else if(multipleResponses == "last") {
                tempvar[j] = i
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = i
                } else {tempvar[j] = paste0(tempvar[j], i, collapse=", ")}
              }
          }
        }
      }
      if (!is.null(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (dat[j,i] == resp)
              if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = i
                  }
              } else if(multipleResponses == "last") {
                tempvar[j] = i
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = i
              } else {tempvar[j] = paste0(tempvar[j], i, collapse=", ")}
          }
          }
        }
      }
    }

    if (cellEntry == "original") {
      if (is.null(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (!is.na(dat[j,i]))
               if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = as.character(dat[j,i])
                  }
              } else if(multipleResponses == "last") {
                tempvar[j] = as.character(dat[j,i])
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = as.character(dat[j,i])
              } else {tempvar[j] = paste0(tempvar[j], as.character(dat[j,i]), collapse=", ")}
          }
          }
        }
      }
      if (!is.null(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (dat[j,i] == resp)
               if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {tempvar[j] = as.character(dat[j,i])}
              } else if(multipleResponses == "last") {
                tempvar[j] = as.character(dat[j,i])
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = as.character(dat[j,i])
              } else {tempvar[j] = paste0(tempvar[j], as.character(dat[j,i]), collapse=", ")}
          }
          }
        }
      }
    }
    if (cellEntry == "column name") {
      if (is.na(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (!is.na(dat[j,i]))
               if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = varList[i]
                  }
              } else if(multipleResponses == "last") {
                tempvar[j] = varList[i]
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = varList[i]
              } else {tempvar[j] = paste0(tempvar[j], varList[i], collapse=", ")}
          }
          }
        }
      }
      if (!is.na(resp)) {
        tempvar <- matrix(NA,ncol = 1,nrow = length(data[,1]))
        dat <- data[,varList]
        for (i in 1:length(varList)) {
          for (j in 1:length(data[,1])) {
            if (dat[j,i] == resp)
              if(multipleResponses == "first") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = varList[i]
                }
              } else if(multipleResponses == "last") {
                tempvar[j] = varList[i]
              } else if(multipleResponses == "combine") {
                if(is.na(tempvar[j])) {
                  tempvar[j] = varList[i]
              } else {tempvar[j] = paste0(tempvar[j], varList[i], collapse=", ")}
          }
          }
        }
      }
    }
    tempvar <- ifelse(tempvar == "NaN",NA,tempvar)
    return(tempvar)
  }
