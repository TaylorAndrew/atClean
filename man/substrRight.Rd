% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/substrRight.R
\name{substrRight}
\alias{substrRight}
\title{substrRight}
\usage{
substrRight(x, n)
}
\arguments{
\item{x}{Character string or vector of character strings}

\item{n}{Number of characters to exract from strings starting with the right-most character.}
}
\value{
A string or vector of strings of length less than or equal to n, consisting of characters on the right side of the strings.
}
\description{
substrRight returns a substring of a string or vector of strings of length less than or equal to a given `n`, starting with the right-most character.
}
\examples{
#x = "OneString"
#x2 = c("ThisIsOne", "ThisIsTwo", "This is three", "And this4")
#Regular substr() usage
#substr(x, start = 1, stop = 3)
#substr(x2, start = 1, stop = 3)
#substrRight usage
#substrRight(x, n = 3)
#substrRight(x2, n = 3)
}

