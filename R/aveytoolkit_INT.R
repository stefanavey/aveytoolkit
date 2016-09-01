##' @title INT
##'
##' @description
##' \code{INT} INT performs an inverse normal transformation
##'
##' @details Takes an input vector and performs a rank-based inverse normal transformation (making the data approximately normally distributed. Positions with missing (NA) values will be returned as NA by default (see `na.last`)
##' 
##' @param x           numeric vector to be transformed
##' @param na.last     How NA values should be handled. Passed to \code{rank}.
##' @param ties.method How ties should be handled. Passed to \code{rank}.
##' @param ...         Other arguments passed to \code{qnorm}
##' @return A numeric vector containing the transformed values of x.
##' 
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @export
##' @examples
##' ## Normally Distributed data
##' x1 <- rnorm(100)
##' hist(INT(x1)) # still normally distributed
##' hist(INT(x1, mean = 10, sd = 2)) # still normally distributed
##'
##' ## Uniformly Distributed data
##' x2 <- runif(100)
##' hist(INT(x2)) # forced to be normally distributed by rank
##'
##' ## Many ties in data, different methods for handling ties
##' x3 <- rep(10:20, 5)
##' hist(INT(x3, ties.method = "average"))
##' hist(INT(x3, ties.method = "first"))
##' hist(INT(x3, ties.method = "max"))
INT <- function(x, na.last = "keep",
                ties.method = c("average", "first", "last",
                    "random", "max", "min"), ...)
{
  ties.method <- match.arg(ties.method)
  xranks <- rank(x, na.last = na.last, ties.method = ties.method)
  tempp <- (xranks-.5) / length(xranks)
  return(qnorm(tempp, ...))
}

