#' sigHeatmap
#'
#' Draw a venn diagram of 2 or 3 sets
#'
#' @param setList a (named) list of the sets to be plotted.  The names will be used on the plot. If the list is unnamed, the default names in \code{\link{vennDiagram}}
#' @return a data frame of binary values indicating membership in each set with rownames giving the set entries.
#' @seealso \code{\link{vennDiagram}}
#' @details Wrapper around the \code{\link{limma}} \code{\link{vennDiagram}} function to make it simpler.
#' @references Code modified from \url{http://research.stowers-institute.org/mcm/venn.R} 
#' @author Stefan Avey
#' @keywords aveytoolkit
#' @export
## Do not need to import vennDiagram only because aveytoolkit already imports all of limma for runLimma
## @importFrom limma vennDiagram 
VennDiagram <- function(setList, mar=c(0,0,1,0), ...) {
  stopifnot( is.list(setList) )
  ## Form universe as union of all three sets
  universe <- sort( Reduce(union, setList) )
  Counts <- matrix(0, nrow=length(universe), ncol=length(setList))
  colnames(Counts) <- names(setList)
  for(j in 1:ncol(Counts)) {
    Counts[,j] <- as.numeric(universe %in% setList[[j]])
  }
  vennDiagram(vennCounts(Counts), names=names(setList), mar=mar, ...)
  tab <- data.frame(Counts, row.names=universe, stringsAsFactors=FALSE)
  if(!is.null(names(setList)))
    colnames(tab)<-c(names(setList))
  else
    colnames(tab) <- paste0("Group", 1:length(setList))
  return(invisible(tab))
}
