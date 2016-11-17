##' @title GetEqn
##'
##' @description
##' \code{GetEqn} gets the equation for various models in a human readable format
##'
##' @details
##' 
##' @references original lm_eqn and inspiration from this SO post \url{http://stackoverflow.com/questions/7549694/ggplot2-adding-regression-line-equation-and-r2-on-graph}.
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @export
##' @examples
##' ## First Example
##' 
##' @param m a model object
GetEqn <- function(m)
{
  .lm_eqn <- function(m) {
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              r2 = format(summary(m)$r.squared, digits = 2));
    
    if (coef(m)[2] >= 0)  {
      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
    } else {
        eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
      }
    as.character(as.expression(eq));
  }

  .nls_eqn <- function(m) {
    l <- list(a = format(coef(m)[1], digits = 2),
              b = format(abs(coef(m)[2]), digits = 2),
              rss = format(deviance(m), digits = 2))

    if (coef(m)[2] >= 0)  {
      eq <- substitute(italic(y) ==~expr(a + b %.% italic(x))*", RSS ="~rss,l)
    } else {
        eq <- substitute(italic(y) ==~exp(a - b %.% italic(x))*", RSS ="~rss,l)
      }
    
    as.character(as.expression(eq));
  }


  if(class(m) == "nls")
    .nls_eqn(m)
  else if (class(m) == "lm")
    .lm_eqn(m)
}



