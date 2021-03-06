% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aveytoolkit_RepeatBefore.R
\name{RepeatBefore}
\alias{RepeatBefore}
\title{RepeatBefore}
\usage{
RepeatBefore(x)
}
\arguments{
\item{x}{a vector of values}
}
\value{
a vector of values
}
\description{
Replaces NAs with the latest non-NA value
}
\details{
NA values will be replaced by the most recent value with a lower index.  If there is no non-NA value before the NA appears, it will remain NA.
}
\examples{
x = c(NA,NA,'a',NA,NA,NA,NA,NA,NA,NA,NA,'b','c','d',NA,NA,NA,NA,NA,'e')
newX <- RepeatBefore(x)
show(newX)
}
\references{
\url{http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value}
}
\seealso{
\code{\link{rep}}
}
\author{
Ruben
}
\keyword{aveytoolkit}
