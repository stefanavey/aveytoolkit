##' @title GetObjectSizes
##'
##' @description
##' \code{GetObjectSizes} uses \code{ls()} and \code{object.size} to see what
##' objects are using most of the memory. \code{lsos()} is better for this purpose.
##' @param name which environment to use in listing the available objects.
##'             Defaults to the _current_ environment.  Although called
##'             ‘name’ for back compatibility, in fact this argument can
##'             specify the environment in any form; see the ‘Details’
##'             of \code{ls()} for more information.
##' @param units the units to be used in printing the size.  Allowed values
##'              are ‘"b"’, ‘"Kb"’, ‘"Mb"’, ‘"Gb"’, ‘"Tb"’, ‘"Pb"’, ‘"B"’,
##'             ‘"KB"’, ‘"MB"’, ‘"GB"’, ‘"TB"’, ‘"PB"’, ‘"KiB"’, ‘"MiB"’,
##'             ‘"GiB"’, ‘"TiB"’, ‘"PiB"’, ‘"EiB"’, ‘"ZiB"’, ‘"YiB"’, and
##'             ‘"auto"’ (see ‘Details’ of \code{object.size}).  Can be abbreviated.
##' @return A named character vector with names corresponding to objects and values
##'         corresponding to strings in human-readable format
##' @details
##' 
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @seealso \code{ls}, \code{object.size}
##' @export
##' @examples
##' ## First Example
##' bigMat <- matrix(NA, nrow = 1000, ncol = 1000)
##' biggerMat <- matrix(NA, nrow = 10000, ncol = 10000)
##' GetObjectSizes()
##' GetObjectSizes(units = "Gb")
GetObjectSizes <- function(name = ".GlobalEnv", units = "Mb")
{
  bytes <- sapply(ls(name = name), function(x) { object.size(get(x)) })
  bytes <- sort(bytes, decreasing = TRUE)
  sizes <- utils:::format.object_size(bytes, units = units)
  names(sizes) <- names(bytes)
  return(sizes)
}

