#' sigHeatmap
#'
#' Draw heatmap with significance indicated on boxes
#'
#' @param hm a matrix of values used for drawing the heatmap
#' @param pvals a list or data frame of (possibly FDR corrected but this is not handled by the function) positive p-values
#' @param pvalDisplayName is printed on the heatmap as a legend.  Default is "P-value" but might want to change to "Q-value", "FDR", etc.
#' @param cutoff is threshold for significance of pvals. Default is 0.05
#' @param showOnly one of "both", "positive", "negative", or "all" can be abbreviated.
#' @param main a string giving the plot main title. Default is "" (i.e. no title is plotted).
#' @param mainNewlines a non-negative integer specifying the number of newline characters to plot before the main title. Used to make the title appear lower on the page. Default is 0
#' @param sigChar the character used for plotting on top of significant boxes
#' @param Rowv should the rows be reordered, passed into heatmap.2
#' @param hclustMethod passed to the function stats::hclust.  The agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Default is "ward.D".
#' @param ... other arguments passed to heatmap.2
#' @return a vector indicating which of the rows of hm were determined to be significant and subsequently plotted
#' @importFrom gplots heatmap.2
#' @details Only rows with at least one significant column are plotted. If showOnly is "both", plots both positive and negative significant changes. If showOnly is "positive" or "negative", plots only rows of hm with significant positive or negative values respectively. If showOnly is "all", all rows of hm are shown.
#' @author Stefan Avey
#' @keywords aveytoolkit
#' @export
#' @examples
#' data(mtcars)
#' x <- as.matrix(mtcars)
#' alpha <- 10^-7 # significance threshold
#' ## Caculate whether difference from mean is significant
#' ## This is not done correctly but just to have some sort of significance
#' diffMean <- mtcars-matrix(colMeans(mtcars),
#'                           ncol=ncol(mtcars), nrow=nrow(mtcars), byrow=TRUE)
#' stdErr <- matrix(sapply(mtcars, sd)/sqrt(nrow(mtcars)),
#'                 ncol=ncol(mtcars), nrow=nrow(mtcars), byrow=TRUE)
#' tstats <- diffMean/stdErr
#' pvals <- pt(as.matrix(tstats), nrow(mtcars)-2, lower=FALSE)
#' op <- par(oma=c(4,0,0,20))
#' sel <- sigHeatmap(x, pvals=pvals, cutoff=alpha, showOnly="b",
#'                   main="mtcars Example Heatmap", sigChar="*", notecol='black',
#'                   notecex=2, Colv=T, Rowv=T, dendrogram="row", trace="none")
#' par(op)
#' ## Which cars weren't selected
#' rownames(mtcars)[setdiff(1:nrow(mtcars), sel)]
sigHeatmap <- function(hm, pvals, pvalDisplayName="P-value", cutoff=0.05,
                       showOnly=c("both", "positive", "negative", "all"), main="",
                       mainNewlines=0, sigChar='*', Rowv=T, hclustMethod="ward.D", ...)
{
  ## Do some checking for conditions we do not want
  if(any(unlist(pvals) < 0) || any(unlist(pvals) > 1))
    stop("pvals should be in interval [0, 1]")
  if(length(sigChar) > 1) {
    sigChar <- sigChar[1]
    warning("sigChar has more than one character, only the first will be used")
  }
#  require(gplots) # for heatmap function
  pvals <- as.data.frame(pvals)         
  showOnly <- match.arg(showOnly)       
  subset <- rep(NA, nrow(pvals))
  switch(showOnly,
         both =  { subset <- apply(pvals, 1, function(row) any(row < cutoff))
                   cn <- pvals[subset,] < cutoff },
         positive = {
           if(!any((pvals < cutoff) & (hm > 0))) # if no values match
             stop("No values to plot: hm contains no positive values or no pvals meet the cutoff")
           for(i in 1:nrow(pvals)) {
             subset[i] <- any(pvals[i,] < cutoff & hm[i,] > 0)
           }
           cn <- pvals[subset,] < cutoff & hm[subset,] > 0 },
         negative = {
           if(!any((pvals < cutoff) & (hm < 0))) # if no values match
             stop("No values to plot: hm contains no negative values or no pvals meet the cutoff")
           for(i in 1:nrow(pvals)) {
             subset[i] <- any(pvals[i,] < cutoff & hm[i,] < 0)
           }
           cn <- pvals[subset,] < cutoff & hm[subset,] < 0 },
         all =  { subset <- rep(TRUE, nrow(pvals))
                  cn <- pvals[subset,] < cutoff }
         )
## cellnote (cn) begins as true or false and is replaced by sigChar or ""
  ## Replace T and F with "*" and "" respectively
  for(i in 1:nrow(cn)) { 
    for(j in 1:ncol(cn)) {
      if(cn[i,j]) {cn[i,j] <- sigChar}
      else {cn[i,j] <- ""}
    }
  }
#  subset2 <- apply(cn, 1, function(row) any(row == sigChar))
  ## hrd is the row dendrogram OR the value passed into the function
  if(Rowv) {
    hrd <- as.dendrogram(stats::hclust(as.dist(1-cor(t(hm[subset,]), method="spearman")),
                                       method="ward.D"))
  } else
    hrd <- Rowv
  if(main != "")
    main <- paste(paste(rep('\n', mainNewlines), collapse=''), main)
  if(pvalDisplayName != "")
    main <- paste0(main, paste(rep('\n', 2), collapse=''), pvalDisplayName, " < ", cutoff)
  heatmap.2(hm[subset,], cellnote=cn, Rowv=hrd, main=main, ...)

  return(invisible(which(subset))) # return subset
}

