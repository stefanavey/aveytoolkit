#' RepeatBefore
#'
#' Replaces NAs with the latest non-NA value
#'
#' @param x a vector of values
#' @return a vector of values 
#' @author Ruben
#' @details NA values will be replaced by the most recent value with a lower index.  If there is no non-NA value before the NA appears, it will remain NA.
#' @keywords aveytoolkit
#' @seealso \code{\link{rep}}
#' @references  \url{http://stackoverflow.com/questions/7735647/replacing-nas-with-latest-non-na-value}
#' @export
#' @examples
#' x = c(NA,NA,'a',NA,NA,NA,NA,NA,NA,NA,NA,'b','c','d',NA,NA,NA,NA,NA,'e')
#' newX <- RepeatBefore(x)
#' show(newX)
RepeatBefore <- function(x) { # repeats the last non NA value. Keeps leading NA
  ind = which(!is.na(x))      # get positions of nonmissing values
  if(is.na(x[1	]))           # if it begins with a missing, add the
    ind = c(1,ind)            # first position to the indices
  rep(x[ind], times = diff(   # repeat the values at these indices
                c(ind, length(x) + 1) )) # diffing the indices + length yields how often
}                                        # they need to be repeated




