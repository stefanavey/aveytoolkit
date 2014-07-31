#' fishersMethod
#'
#' This function combines multiple p-values according to Fisher's Method
#'
#' @return a single combined p-value
#' @author Mike Love
#' @keywords aveytoolkit
#' @references \url{http://mikelove.wordpress.com/2012/03/12/combining-p-values-fishers-method-sum-of-p-values-binomial/}
#' @export
#' @examples
#' x <- c(runif(1000, 0, 1),runif(100,.1,.2))
#' fishersMethod(x)
fishersMethod <- function(x) pchisq(-2 * sum(log(x)),df=2*length(x),lower=FALSE)

