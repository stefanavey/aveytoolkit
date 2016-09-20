##' @title +.uneval
##'
##' @description
##' \code{+.uneval} is a helper function to allow adding aes and aes_string in ggplot2
##'
##' @details
##' 
##' 
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @references \url{http://stackoverflow.com/questions/28777626/how-do-i-combine-aes-and-aes-string-options}
##' @export
##' @examples
##' v1 <- "mpg"
##' v2 <- "qsec"
##' ggplot(mtcars, aes(x=wt)) + ylab("") +
##'    geom_line(aes_string(y=v1) + aes(color="one")) +
##'    geom_line(aes_string(y=v2) + aes(color="two")) +
##'    scale_color_manual(name="Val", values=c(one="#105B63",two="#BD4932"))
`+.uneval` <- function(a,b) {
  `class<-`(modifyList(a,b), "uneval")
}
