% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aveytoolkit_sigHeatmap.R
\name{sigHeatmap}
\alias{sigHeatmap}
\title{sigHeatmap}
\usage{
sigHeatmap(
  hm,
  pvals,
  pvalDisplayName = "P-value",
  cutoff = 0.05,
  showOnly = c("both", "positive", "negative", "all"),
  main = "",
  mainNewlines = 0,
  sigChar = "*",
  Rowv = T,
  hclustMethod = "ward.D",
  ...
)
}
\arguments{
\item{hm}{a matrix of values used for drawing the heatmap}

\item{pvals}{a list or data frame of (possibly FDR corrected but this is not handled by the function) positive p-values}

\item{pvalDisplayName}{is printed on the heatmap as a legend.  Default is "P-value" but might want to change to "Q-value", "FDR", etc.}

\item{cutoff}{is threshold for significance of pvals. Default is 0.05}

\item{showOnly}{one of "both", "positive", "negative", or "all" can be abbreviated.}

\item{main}{a string giving the plot main title. Default is "" (i.e. no title is plotted).}

\item{mainNewlines}{a non-negative integer specifying the number of newline characters to plot before the main title. Used to make the title appear lower on the page. Default is 0}

\item{sigChar}{the character used for plotting on top of significant boxes}

\item{Rowv}{should the rows be reordered, passed into heatmap.2}

\item{hclustMethod}{passed to the function stats::hclust.  The agglomeration method to be used. This should be (an unambiguous abbreviation of) one of "ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC). Default is "ward.D".}

\item{...}{other arguments passed to heatmap.2}
}
\value{
a vector indicating which of the rows of hm were determined to be significant and subsequently plotted
}
\description{
Draw heatmap with significance indicated on boxes
}
\details{
Only rows with at least one significant column are plotted. If showOnly is "both", plots both positive and negative significant changes. If showOnly is "positive" or "negative", plots only rows of hm with significant positive or negative values respectively. If showOnly is "all", all rows of hm are shown.
}
\examples{
data(mtcars)
x <- as.matrix(mtcars)
alpha <- 10^-7 # significance threshold
## Caculate whether difference from mean is significant
## This is not done correctly but just to have some sort of significance
diffMean <- mtcars-matrix(colMeans(mtcars),
                          ncol=ncol(mtcars), nrow=nrow(mtcars), byrow=TRUE)
stdErr <- matrix(sapply(mtcars, sd)/sqrt(nrow(mtcars)),
                ncol=ncol(mtcars), nrow=nrow(mtcars), byrow=TRUE)
tstats <- diffMean/stdErr
pvals <- pt(as.matrix(tstats), nrow(mtcars)-2, lower=FALSE)
op <- par(oma=c(4,0,0,20))
sel <- sigHeatmap(x, pvals=pvals, cutoff=alpha, showOnly="b",
                  main="mtcars Example Heatmap", sigChar="*", notecol='black',
                  notecex=2, Colv=T, Rowv=T, dendrogram="row", trace="none")
par(op)
## Which cars weren't selected
rownames(mtcars)[setdiff(1:nrow(mtcars), sel)]
}
\author{
Stefan Avey
}
\keyword{aveytoolkit}
