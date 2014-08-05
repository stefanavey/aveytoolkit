#' resetPar
#'
#' Simple function to reset plotting parameters for when things get wonky
#'
#' @return an invisible named list of parameters returned by calling par
#' @author Gavin Simpson
#' @details This function resets the graphical parameters from the par function. It flashes a new device on the screen but works to reset parameters.  Meant to be used when things get hairy and not coded in scripts
#' @keywords aveytoolkit
#' @seealso \code{\link{par}}
#' @references \url{http://stackoverflow.com/questions/5789982/reset-par-to-the-default-values-at-startup}
#' @export
#' @examples
#' par(oma=c(4,10,2,1))
#' plot(1,1)
#' ## paramter settings weren't saved so do a reset
#' resetPar()
#' plot(1,1)
resetPar <- function() {
  dev.new()
  op <- par(no.readonly = TRUE)
  dev.off()
  par(op)
}


