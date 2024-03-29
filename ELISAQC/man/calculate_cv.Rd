% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_cv.R
\name{calculate_cv}
\alias{calculate_cv}
\title{Calculate coefficient of variation as a percentage}
\usage{
calculate_cv(values)
}
\arguments{
\item{values}{vector of values to calculate the cv for}
}
\value{
value (coefficient of variation as a percentage)
}
\description{
This function takes a vector of values and returns the coefficient of
variation as a percentage.
}
\examples{
 # Create vector of values:
 x <- c(2.19, 2.32)
 y <- c(0.50, 1.50)
 
 # Calculate cvs:
 xcv <- calculate_cv(x)
 ycv <- calculate_cv(y)
 
}
\author{
Amy Mikhail, \email{amy.mikhail@gmail.com}
}
