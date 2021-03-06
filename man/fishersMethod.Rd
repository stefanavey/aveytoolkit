% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aveytoolkit_fishersMethod.R
\name{fishersMethod}
\alias{fishersMethod}
\title{fishersMethod}
\usage{
fishersMethod(x)
}
\value{
a single combined p-value
}
\description{
This function combines multiple p-values according to Fisher's Method
}
\examples{
x <- c(runif(1000, 0, 1),runif(100,.1,.2))
fishersMethod(x)
}
\references{
\url{http://mikelove.wordpress.com/2012/03/12/combining-p-values-fishers-method-sum-of-p-values-binomial/}
}
\author{
Mike Love
}
\keyword{aveytoolkit}
