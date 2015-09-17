##' MakeDF
##'
##' Creates a data frame from a list.  Useful for when the list elements have unequal lengths and
##' \code{\link{as.data.frame}} fails.
##'
##' @param list the list to convert
##' @param names the names of the list
##' @return a data frame of the converted list.
##' @keywords aveytoolkit
##' @seealso \code{\link{gsub}}
##' @references \url{http://stackoverflow.com/questions/15753091/convert-mixed-length-named-list-to-data-frame}
##' @author Josh O'Brien
##' @export
##' @examples
##' ## Test timing with a 50k-item list
##' ll <- createList(50000)
##' nms <- c("a", "b", "c")
##' 
##' system.time(makeDF(ll, nms))
##' # user  system elapsed
##' # 0.47    0.00    0.47 

MakeDF <- function(list, names) {
  m <- t(vapply(list,
                FUN = function(X) unlist(X)[names],
                FUN.VALUE = numeric(length(names))))
  as.data.frame(m)
}

