##' @title getBaseTheme
##'
##' @description
##' \code{getBaseTheme} Defines universal plotting settings to use with ggplot2
##'
##' @import ggplot2
##' 
##' @author Jason Vander Heiden <jason.vanderheiden@@yale.edu> 
##' @keywords aveytoolkit
##' @export
##' @examples
##' p <- ggplot(mtcars, aes(wt, mpg))
##' p <- p + geom_point() + getBaseTheme()
##' plot(p)

getBaseTheme <- function( ) {
  ## Define universal plot settings
  base_theme <- theme_bw() +
    theme(text=element_text(size=14)) +
      theme(plot.title=element_text(size=16)) +
        theme(strip.background=element_rect(fill='white')) +
          theme(strip.text=element_text(size=16, face='bold'))
  ## theme(axis.title=element_text(size=16, vjust=0.5)) +
  ##   theme(axis.text.x=element_text(size=14, vjust=0.5, hjust=0.5)) +
  ##     theme(axis.text.y=element_text(size=14))
  return(base_theme)
}


