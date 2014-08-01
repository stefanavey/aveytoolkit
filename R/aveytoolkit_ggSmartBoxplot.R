#' ggSmartBoxplot
#'
#' Boxplot wrapper for ggplot
#'
#' @param x the variable to group by for boxplots
#' @param mat data.frame or matrix of values to plot with samples in columns
#' @param splitBy a factor used to split the data into separate plots
#' @param colorBy a factor used for coloring
#' @param rows row names or row indices of the items to be plotted
#' @param cols substring to search for with "grep" in column names to be plotted
#' @param whichCols the column indices or full column names
#' @param sep a separator used in searching for cols in the column names
#' @param filename the name of a file to write a PDF to or \code{NA} to plot in standard graphics device.
#' @param ... other arguments that are passed to qplot
#' @import ggplot2
#' @importFrom grid unit
#' @author Stefan Avey
#' @keywords aveytoolkit
#' @seealso \code{\link{ggplot2}}, \code{\link{qplot}}
#' @export
#' @examples
#' data(OrchardSprays)
#' ## Example of functionality
#' ggSmartBoxplot(x=OrchardSprays$treatment,
#'                mat=t(OrchardSprays[,1]),
#'               rows=1, whichCols=1:ncol(t(OrchardSprays)),
#'               colorBy=factor(OrchardSprays$rowpos+OrchardSprays$colpos > 9),
#'               splitBy=NA, xlab="Treatment")
#'
#' ## NOT RUN:
#' cellType <- "PBMC"
#' geneSub <- grep("HLA-A29.1", rownames(expr))
#' age <- "Young"
#' ages <- c("Young", "Old")
#' responses <- c("NR", "R")
#' subset <- targetFClist[[cellType]]$Age %in% ages & 
#'   targetFClist[[cellType]]$Response %in% responses
#' ggSmartBoxplot(x=targetFClist[[cellType]][subset, "Time"],
#'                mat=exprFClist[[cellType]], ylim=c(-1,1),
#'                rows=geneSub, whichCols=which(subset), 
#'                colorBy=targetFClist[[cellType]][subset,"Response"],
#'                splitBy=targetFClist[[cellType]][subset,"Age"],
#'                xlab="Days (Post Vaccination)",
#'                fileName=NA)
ggSmartBoxplot <- function(x, mat, splitBy=NA, colorBy=NA, rows, cols=NA,
                           whichCols=NA, sep='.', fileName=NA, ...)  {
#  require(ggplot2)
  if(is.character(fileName))
    pdf(fileName)
  if(all(is.na(colorBy)))
    colorBy <- factor(rep(1, ncol(mat)))
  for(r in rows) {
    if(is.na(cols) && !is.na(whichCols)) {
      submat <- mat[r,whichCols]
      ## Find the common string separated  sep in the column names
    } else if (is.na(whichCols) && !is.na(cols)) {
      submat <- mat[r,grep(paste0(sep, cols, sep), colnames(mat), fixed=T)]
    }
    else
      stop("Must specify columns in cols or whichCols argument")
    ## x2 is defined 
    x2 <- as.numeric(as.factor(x)) +
      0.25*seq(from=-1, to=1,
               length.out=length(levels(factor(colorBy))))[as.numeric(factor(colorBy))]
    dat <- data.frame(x=factor(x), x2=x2, vals=unlist(submat), splitBy=splitBy,
                      colorBy=factor(colorBy))
#      print(dat)
    ## Assign rownames as r or the rownames at row number r
    if(is.numeric(r))
      rowName <- rownames(mat)[r]
    else
      rowName <- r
#f <- ggplot(dat, aes(x=x, y=vals)) +                                       
#        scale_x_discrete() +
#          geom_boxplot(aes(fill=colorBy), color=c("yellow", "blue"), outlier.shape=NA,
#                       position=position_dodge(1)) +
    f <- qplot(x, vals, data = dat, geom="boxplot", ylab=rowName, fill=colorBy,
               position=position_dodge(1),
               outlier.shape=NA,
               ...) +
                 geom_jitter(aes(x=x2), position=position_jitter(width=0.1)) +
                   theme_bw() +
                     theme(plot.margin=unit(c(5, 0, 5, 10), "mm"),
                           text=element_text(size=16),
                           axis.title.x = element_text(vjust=-.75),
                           axis.title.y = element_text(vjust=0))
    ## Default colors for only 2 classes are yellow and blue
    if(length(levels(colorBy)) == 2)
      f <- f + scale_fill_manual(name="", breaks=factor(colorBy), values=c("yellow", "blue"))
    ## only split if splitBy is not NA
    if(!all(is.na(splitBy)))
      f <- f + facet_grid(.~splitBy, scales="free", space="free")
    ## No legend if only 1 color is used - does not work
#    if(length(unique(colorBy)) == 1)
#      f <- f + guides(color=FALSE)
    plot(f)
  }
  if(is.character(fileName))
    dev.off()
}


