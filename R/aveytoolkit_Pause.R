#' Pause
#'
#' This function prompts for return key and waits until the return is pushed to continue execution. It is used often to view plots coded in a loop one at a time allowing the user to control when the next plot should be displayed
#'
#' @param str optional string to display. Defaults to "continue".
#' @param quiet if TRUE, no prompt is displayed. Default is FALSE
#' @return NULL is returned by invisible
#' @details The Pause function uses readline to wait until a newline character (produced by the Enter key) is given.  Instead of pressing Enter, a newline character can be used to automate this waiting time.
#' @author Stefan Avey
#' @seealso \code{\link{readline}}, \code{\link{invisible}}
#' @keywords aveytoolkit
#' @export
#' @examples
#' for(p in 1:10) {
#'   plot(-10:10, (-10:10)^p, type='b')
#'   Pause(paste0('see plot of x^',p+1))
#' }
Pause <- function (str="continue", quiet=FALSE) {
  if(quiet)
    prompt <- ""
  else
    prompt <- paste("Press <Enter> to ", str, "... ", sep="")
  readline(prompt=prompt)
  invisible()
}

