##' resetPar
##'
##' Calculate the geometric mean
##'
##' @param x a vector of numeric values
##' @param na.rm (optional) whether to remove \code{NA} values before calculation. Default is \code{TRUE}
##' @return the geometric mean of x
##' @author Paul McMurdie
##' @details This function handles 0 values by ignoring them
##' @keywords aveytoolkit
##' @seealso \code{\link{exp}} \code{\link{sum}} \code{\link{log}}
##' @references \url{http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}
##' @export
##' @examples
##' x <- 1:10
##' x2 <- x^2
##' geomMean(x)
##' mean(x)
##' geomMean(x2)
##' mean(x2)
geomMean <- function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
