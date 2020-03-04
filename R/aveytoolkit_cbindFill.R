##' resetPar
##'
##' Simple function to combine multiple objects by column while filling in NAs into extra rows created from differing lengths
##'
##' @param ... the objects, that will be converted to a list, to bind column-wise
##' @return the cbind'ed objects passed in
##' @author Dimitris Rizopoulos and Tyler Rinker
##' @keywords aveytoolkit
##' @seealso \code{\link{cbind}}
##' @references \url{http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill}
##' @export
##' @examples
##' 
##' x <- matrix(1:10, 5, 2)
##' y <- matrix(1:16, 4, 4)
##' z <- matrix(1:12, 2, 6)
##' 
##' cbind.fill(x,y)
##' cbind.fill(x,y,z)
##' cbind.fill(mtcars, mtcars[1:10,])
cbind.fill <- function(...) {
  nm <- list(...)
  nm <- lapply(nm, as.matrix)
  n <- max(sapply(nm, nrow))
  do.call(cbind, lapply(nm, function (x)
                        rbind(x, matrix(, n-nrow(x), ncol(x)))))
}
