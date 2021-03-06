% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/applyLabels.R
\name{applyLabels}
\alias{applyLabels}
\title{applyLabels}
\usage{
applyLabels(data, varlist, labels, fullLevels = NULL)
}
\arguments{
\item{data}{Input data.frame that contains the to-be labeled variable(s)}

\item{varlist}{Vector of to-be labled variable names}

\item{labels}{Vector of labels to apply to the selected variable(s)}

\item{fullLevels}{if the length of labels does not match the number of levels found in data, this will provide the 'theoretical' full levels that 'labels' was based on}
}
\value{
A data.frame with labels added to the variables listed in varlist
}
\description{
applyLabels will take either a single variable or a list of variables and apply a set of labels.
}
\examples{
#fakeData <- data.frame(x = sample(c(0, 1), 10, replace=T))
#applyLabels(data = fakeData, varlist = c("x"), labels = c("No", "Yes"))
}

