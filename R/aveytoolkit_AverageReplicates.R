#' AverageReplicates
#'
#' This function averages replicates in a matrix or data.frame
#'
#' @param eSubSet a matrix or data.frame of values with samples as columns
#' @param numRep the number of replicates
#' @return a data.frame of averaged values with column names coming from the first of each of the replicates with .avg appended
#' @author Stefan Avey
#' @note Assumes that the replicates are all next to each other
#' @keywords aveytoolkit
#' @export
#' @examples
#' mat <- matrix(rnorm(1000), ncol=10) ## 10 columns of random uniform numbers
#' avgMat <- AverageReplicates(mat, numRep=2) ## average adjacent pairs of columns
AverageReplicates <- function(eSubSet, numRep) {
  avgESubSet <- t(apply(eSubSet, 1, function(row)
                        colMeans(matrix(row, nrow=numRep, ncol=ncol(eSubSet)/numRep))))
  if(!is.null(colnames(eSubSet)))
    colnames(avgESubSet) <- paste(colnames(eSubSet)[seq(1,ncol(eSubSet), numRep)],
                                  "avg", sep=".")
  else # there are no column names so use numbers
    colnames(avgESubSet) <- paste(1:(ncol(eSubSet)/numRep), "avg", sep=".")
  return(avgESubSet)
}


AverageReplicates2 <- function(eSubSet, numRep) {
  avgESubSet <- data.frame(matrix(nrow=nrow(eSubSet), ncol=ncol(eSubSet)/numRep),
                           row.names=rownames(eSubSet))
  if(!is.null(colnames(eSubSet)))
    colnames(avgESubSet) <- paste(colnames(eSubSet)[seq(1,ncol(eSubSet), numRep)],
                                  "avg", sep=".")
  else # there are no column names so use numbers
    colnames(avgESubSet) <- paste(1:(ncol(eSubSet)/numRep), "avg", sep=".")
  for(row in 1:nrow(eSubSet))
    avgESubSet[row,] <- colMeans(matrix(as.numeric(eSubSet[row,]),
                                        nrow=numRep, ncol=ncol(eSubSet)/numRep))
  return(avgESubSet)
}





