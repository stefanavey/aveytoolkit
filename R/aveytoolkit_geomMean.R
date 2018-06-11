##' geomMean
##'
##' Calculate the geometric mean
##'
##' @param x a vector of non-negative numeric values. 
##' @param na.rm (optional) whether to remove \code{NA} values before calculation. Default is \code{FALSE}
##' @param zero.propagate (optional) logical specifying whether zeros should be progated such that if there are zeros in `x` 0 should be returned.
##' @return the geometric mean of x
##' @author Paul McMurdie, Stefan Avey
##' @details This function handles negative values by returning `NaN` and zeros by returning `0` by default.
##' @keywords aveytoolkit
##' @seealso \code{\link{exp}} \code{\link{sum}} \code{\link{log}}
##' @references \url{http://stackoverflow.com/questions/2602583/geometric-mean-is-there-a-built-in}
##' @export
##' @examples
##' x <- 1:10
##' x2 <- c(x, NA)
##' x3 <- -5:5
##' x4 <- 0:4
##' 
##' geomMean(x)
##' mean(x)
##' 
##' geomMean(x2)
##' mean(x2)
##'
##' geomMean(x2, na.rm = TRUE)
##' mean(x2, na.rm = TRUE)
##' 
##' ## NaN because because x3 contains negative values ##
##' geomMean(x3)
##' mean(x3)
##'
##' geomMean(x4)
##' geomMean(x4, zero.propagate = FALSE)
##' mean(x4)
geomMean <- function(x, na.rm = FALSE, zero.propagate = TRUE){
    if (any(x < 0, na.rm = TRUE)){
        return(NaN)
    }
    if (zero.propagate) {
        if (any(x == 0, na.rm = TRUE)){
            return(0)
        }
        exp(mean(log(x), na.rm = na.rm))
    } else {
        exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
}
