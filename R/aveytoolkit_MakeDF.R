##' MakeDF
##'
##' Creates a data frame from a list.  Useful for when the list elements have unequal lengths and
##' \code{\link{as.data.frame}} fails.
##'
##' @param List the list to convert
##' @param Names the names of the list
##' @return a data frame of the converted list.
##' @keywords aveytoolkit
##' @seealso \code{\link{gsub}}
##' @references \url{http://stackoverflow.com/questions/15753091/convert-mixed-length-named-list-to-data-frame}
##' @author Josh O'Brien
## TODO: Fix this function, doesn't seem to work
MakeDF <- function(List, Names) {
    m <- t(vapply(List, 
                  FUN = function(X) unlist(X)[Names], 
                  FUN.VALUE = numeric(length(Names))))
    as.data.frame(m)
}
