##' geomMean
##'
##' Calculate the geometric mean
##'
##' @param x a vector of positive numeric values.  
##' @param na.rm (optional) whether to remove \code{NA} values before calculation. Default is \code{TRUE}
##' @return the geometric mean of x
##' @author Paul McMurdie
##' @details This function handles negative or 0 values by warning that they are ignored and calculating the geometric mean without them
##' @keywords aveytoolkit
##' @seealso \code{\link{exp}} \code{\link{sum}} \code{\link{log}}
##' @references \url{http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}
##' @export
##' @examples
##' x <- 1:10
##' x2 <- x^2
##' x3 <- -5:5
##' 
##' geomMean(x)
##' mean(x)
##' 
##' geomMean(x2)
##' mean(x2)
##' 
##' ## Warning because x3 contains negative values ##
##' geomMean(x3)
##' mean(x3)

geomMean <- function(x, na.rm=TRUE){
  if(any(x <= 0)) {
    warning("Some values of x are not positive, only positive values will be used")
  }
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
