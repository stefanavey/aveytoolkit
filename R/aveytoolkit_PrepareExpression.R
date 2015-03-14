##' PrepareExpression
##'
##' Takes in an expression matrix or data frame and prepares it for further analysis
##'
##' @param eset expression information and (potentially) other columns
##' @param target target file where the column names of eset can be matched to `Label`
##' @param returnProbes whether probe mapping should be returned along with expression values in a list. This will only be returned correctly if there is a column of eset matching SYMBOL in any case. 
##' @param labelColumn the column name in the target file to use for matching the column names of eset. Default is "Label"
##' @param select the column names of target to select and merge as the new column names of eset
##' @param collapse 
##' @return if returnProbes is FALSE: a list of the prepared expression data frame (exprDat) and the (potentially modified) target data frame (target) .  if returnProbes is TRUE (default): a list of three elements including the two above and probeMap (a vector mapping from gene symbols to probe names).
##' @details Wrote this to automate the few lines I always perform to "prepare" an expression set for further processing.  I always want to remove the symbols column, rename the column names based on the target file, and (usually) change the rownames to be gene symbols.  This function takes in the matrix format that I use to store processed expression files (in a pacakge or file).
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @export
##' @examples
##' ## Creating fake expression matrix
##' dat <- matrix(rnorm(1000, mean=8, sd=1), nrow=100, ncol=10)
##' colnames(dat) <- sample(letters[1:10], size=10)
##' fakeGenes <- as.vector(outer(LETTERS[1:26], LETTERS[1:26], paste0))
##' x <- data.frame(symbol=fakeGenes[1:nrow(dat)], dat, row.names=paste0("Probe_", 1:nrow(dat)))
##' head(x) # look at first 6 rows of toy data set
##' target <- data.frame(Label=letters[1:26], Class=rep(1:3, length.out=26))
##' head(target)
##' 
##' prepList <- PrepareExpression(x, target, select="Class")
##' head(prepList$exprDat)
##' head(prepList$target)
##' head(prepList$probeMap)
##'
##' \dontrun{
##' ## Load expression data from HIPC package
##' library(HIPC)
##' data(y3ExprPBMC, y3Target)
##' prepList <- PrepareExpression(y3ExprPBMC, y3Target,
##'                               select=c("Response", "SubjectID", "Age", "Time"))
##' }
PrepareExpression <- function(eset, target, returnProbes=TRUE, labelColumn="Label",
                              select=colnames(target), collapse='.')
{
  exprDat <- as.data.frame(eset)
  symbolSearch <- grepl("SYMBOL", toupper(colnames(eset)), fixed=TRUE)
  if(sum(symbolSearch) > 1) {
    stop("More than 1 column of eset matches 'SYMBOL'")
  } else if (sum(symbolSearch) == 1) {
    ## Create mapping to current column names
    probeMap <- rownames(exprDat)
    names(probeMap) <- exprDat[,symbolSearch]
    ## Replace row names with symbols
    rownames(exprDat) <- names(probeMap)
    ## Remove symbol column
    exprDat <- exprDat[,-which(symbolSearch)]
  }
  ## Find the index of each element of target_labels in colname_label 
  matchIndex <- match(target[[labelColumn]], colnames(exprDat))
  if(any(is.na(matchIndex))) { # remove any NA values
    matchIndex <- na.omit(matchIndex)
    target <- target[-na.action(matchIndex),]
    warning("Some rows of target$", labelColumn, " do not match the column names of eset.\n",
            "The following rows have been removed from the target:\n",
            paste(na.action(matchIndex), collapse=', '))
  }
  exprDat <- exprDat[,matchIndex]
  ## Use selected columns and collapse together with `collapse`
  ## to form column names of exprDat
  colnames(exprDat) <- apply(sapply(target, as.character), 1,
                             function(row) { paste(row[select], collapse=collapse) } )
  if(returnProbes) {  # Return expression data frame, target and probeMap
    if(!exists("probeMap")) {
      probeMap <- NULL
    }
    return(list(exprDat=exprDat, target=target, probeMap=probeMap))
  } else {  # Return expression data frame and target
    return(list(exprDat=exprDat, target=target))
  }
}


