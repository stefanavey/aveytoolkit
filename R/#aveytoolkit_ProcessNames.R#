#' ProcessNames
#'
#' Cleans up strings to make them pretty names by removing punctuation, whitespace, and specified substrings
#'
#' @param strs vector or strings to process
#' @param stringsToRm a vector or list of strings to search for and remove from strs
#' @param rmPunct should punctuation be removed? Default is TRUE.
#' @param sep character to replace whitespace
#' @return a vector of modified strings from strs
#' @author Stefan Avey
#' @details stringsToRm are replaced by '' in the order they are given using gsub. After this, punctuation is removed if rmPunct is TRUE. Then, leading and/or trailing whitespace will be removed and the sep will be used to separate words. This function is useful when reading in other people's data and you want to change the row or column names to legal R names or just shorten the names.
#' @keywords aveytoolkit
#' @seealso \code{\link{gsub}}
#' @export
#' @examples
#' badNames <- c("Who's Birthday?", "[Date]", "gift Received")
#' ## Remove the string "Who's", remove punctuation, and separate words by '_'
#' goodNames <- ProcessNames(badNames,stringsToRm="Who's", rmPunct=TRUE, sep='_')
#' goodNames
#' ## Remove the string "Who's", don't remove punctuation, and put no separation between words
#' goodNames <- ProcessNames(badNames,stringsToRm="Who's", rmPunct=FALSE, sep='')
#' goodNames                          
ProcessNames <- function(strs, stringsToRm=NULL, rmPunct=TRUE, sep="_")  {
  for (str in stringsToRm)
    strs <- gsub(str, '', strs) # remove strings in stringsToRm
  if(rmPunct) # remove punctuation
    strs <- gsub("[[:punct:]]", '', strs)
  strs <- gsub("^[[:space:]]+|[[:space:]]+$", '', strs) # remove leading/trailing whitespace
  strs <- gsub('[[:space:]]+', sep, strs, fixed=FALSE) # change spaces to underscores
  return(strs)
}


