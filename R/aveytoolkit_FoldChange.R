#' FoldChange
#'
#' Calculate the fold change between pairs of conditions
#'
#' @param hm a matrix of values used for drawing the heatmap
#' @param pvals a list or data frame of (possibly FDR corrected but this is not handled by the function) positive p-values
#' @param pvalDisplayName is printed on the heatmap as a legend.  Default is "P-value" but might want to change to "Q-value", "FDR", etc.
#' @param cutoff is threshold for significance of pvals. Default is 0.05
#' @param showOnly one of "both", "positive", "negative", or "all" can be abbreviated.
#' @param main a string giving the plot main title. Default is "" (i.e. no title is plotted).
#' @return a data.frame of the fold changes with one column for each fold change
#' @details
#' @author Stefan Avey
#' @keywords aveytoolkit
#' @export
#' @examples

FoldChange <- function(x, condNum, condDen, grouping, labels) {


  exprFC <- data.frame(matrix(nrow=nrow(expr), ncol=ncol(expr) - 1/4 * ncol(expr)))
  rownames(exprFC) <- rownames(expr)
  lcv <- 1
  for(sub in unique(target$SubjectID)) {
    ## This works as long as the baseline measurement is not missing
    if(0 %in% target$Time[target$SubjectID == sub]) {
      ind <- grep(sub, colnames(expr))
      exprFC[,lcv:(lcv+length(ind)-2)] <- expr[,ind[2:length(ind)]] - expr[,ind[1]]
      colnames(exprFC)[lcv:(lcv+length(ind)-2)] <- colnames(expr)[ind[2:length(ind)]]
      lcv <- lcv + length(ind)-1
    }
  }





}
