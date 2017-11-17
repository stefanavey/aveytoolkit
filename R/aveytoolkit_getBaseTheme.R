##' @title getBaseTheme
##'
##' @description
##' \code{getBaseTheme} Defines universal plotting settings to use with ggplot2
##'
##' @import ggplot2
##' 
##' @author Jason Vander Heiden <jason.vanderheiden@@yale.edu>, Stefan Avey <stefan.avey@yale.edu>
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
        theme(plot.title=element_text(size=18)) +
        theme(strip.background=element_rect(fill='white')) +
        theme(strip.text=element_text(size=16, face='bold')) +
        theme(plot.title = element_text(hjust = 0.5)) +
        ## theme(axis.text.x=element_text(size=14, vjust=0.5, hjust=0.5)) +
        ## theme(axis.text.y=element_text(size=14)) +
        theme(axis.title=element_text(size=16, vjust=0.5)) +
        theme(plot.caption = element_text(size = 10, face = "italic"))
    return(base_theme)
}


