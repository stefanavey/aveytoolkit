##' FoldChange
##'
##' Calculate the fold change between pairs of conditions in a matrix or data frame
##'
##' @param x matrix or data.frame from which to calculate fold changes with samples in columns.
##' @param condNum a vector of condition(s) to be used as the numerator in the fold change calculation
##' @param condDen a vector of condition(s) to be used as the denominator in the fold change calculation
##' @param conditions a vector with length equal to the number of columns of x containing the condition labels between which to find the fold changes.
##' @param grouping a vector with length equal to the number of columns of x containing a grouping of the samples (e.g. subjects, cell lines, strains).
##' @param preserveOrder if TRUE, the same ordering of the columns in x will be kept after columns in condDen are removed. If FALSE (the default for backwards compatability), the ordering is changed to sort by group, then by condNum in order passed in.
##' @param log2Transform when 'TRUE', log2 transformation will be applied to x before taking the FC. If 'FALSE' (default) no transformation is applied and x is ASSUMED to be already log transformed.
##' @return a data.frame of the fold changes with one column for each fold change
##' @details FoldChange takes the fold change of log2 transformed data by subtracting columns of the x dataframe or matrix depending on the conditions passed in.
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @export
FoldChange <- function(x, condNum, condDen, conditions, grouping, preserveOrder=FALSE, log2Transform=FALSE) {
  if(length(grouping) != ncol(x)) {
    stop("grouping does not have the same number of elements as the columns (samples) of x.")
  }
  if(length(conditions) != ncol(x)) {
    stop("conditions does not have the same number of elements as the columns (samples) of x.")    
  }
  if( !(all(c(condNum, condDen) %in% conditions)) ) {
    stop("condNum or condDen contain values not in conditions.")
  }
  xFC <- data.frame(matrix(nrow=nrow(x), ncol=ncol(x)), row.names=rownames(x))
  if(log2Transform) { x <- log2(x) }
  ccv <- 1
  for(g in unique(grouping)) {
    group <- which(grouping == g)
    for(cd in condDen) {
      colD <- intersect(which(conditions == cd), group)
      for(cn in condNum) {
        colN <- intersect(which(conditions == cn), group)
        if(length(colN) == 1 && length(colD) == 1) {
          newIndex <- ifelse(preserveOrder, colN, ccv)
          xFC[,newIndex] <- x[,colN] - x[,colD]
          colnames(xFC)[newIndex] <- paste(g, paste(cn, cd, sep='-'), sep='.')
          ccv <- ccv + 1 # incrememt column count variable
        } else if(length(colN) > 1 || length(colD) > 1) {
          warning("Multiple matches for ", paste(g, paste(cn, cd, sep='-'), sep='.'),
                  "\n\tNo fold change calculated")
        }
      }
    }
  }
  xFC <- Filter(function(a) !all(is.na(a)), xFC) # remove extra NA columns
  return(xFC)
}
