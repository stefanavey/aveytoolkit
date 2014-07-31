#' makeTransparent
#'
#' Simple function to make some colors transparent
#'
#' @param ... vector or list of colors
#' @param alpha transparency factor in range [0,1]
#' @return a vector of new colors made transparent
#' @author Ricardo Oliveros-Ramos
#' @keywords aveytoolkit
#' @seealso \code{\link{rgb}}, \code{\link{col2rgb}}
#' @references \url{http://stackoverflow.com/questions/8047668/transparent-equivalent-of-given-color}
#' @export
#' @examples
#' makeTransparent("red", "blue")
#' ##[1] "#FF00007F" "#0000FF7F"
#' makeTransparent("red", "blue", alpha=0.8)
#' ## [1] "#FF0000CC" "#0000FFCC"
makeTransparent = function(..., alpha=0.5) {
  if(alpha<0 || alpha>1) stop("alpha must be between 0 and 1")
  alpha = floor(255*alpha)
  newColor = col2rgb(col=unlist(list(...)), alpha=FALSE)

  .makeTransparent = function(col, alpha) {
    rgb(red=col[1], green=col[2], blue=col[3], alpha=alpha, maxColorValue=255)
  }

  newColor = apply(newColor, 2, .makeTransparent, alpha=alpha)
  return(newColor)
}



