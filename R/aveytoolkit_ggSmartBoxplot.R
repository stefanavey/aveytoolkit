##' ggSmartBoxplot
##'
##' Boxplot wrapper for ggplot
##'
##' @param x the variable to group by for boxplots
##' @param mat data.frame or matrix of values to plot with samples in columns
##' @param splitRowBy a factor used to split the data by row in facet_grid
##' @param splitColBy a factor used to split the data by col in facet_grid
##' @param colorBy a factor used for coloring. No coloring will be done if \code{NULL} (default)
##' @param rows row names or row indices of the items to be plotted
##' @param cols substring to search for with "grep" in column names to be plotted
##' @param whichCols the column indices or full column names
##' @param sep a separator used in searching for cols in the column names
##' @param outlier.shape shape of outliers (default is 17, filled triangle)
##' @param outlier.color color of outliers (default is NULL, i.e. they will not be colored differently)
##' @param ylab if NULL, default is to use rownames. Can specify a string instead to use
##' @param space If \code{"fixed"}, the default, all panels have the same size.  If \code{"free_y"} their height will be proportional to the length of the y scale; if \code{"free_x"} their width will be proportional to the length of the x scale; or if \code{"free"} both height and width will vary.  This setting has no effect unless the appropriate scales also vary.
##' @param scales Are scales shared across all facets (the default, \code{"fixed"}), or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})
##' @param fileName 
##' @param plot logical specifying whether or not to plot the plot(s). Default is TRUE.
##' @param ... other arguments that are passed to qplot
##' @param filename the name of a file to write a PDF to or \code{NA} to plot in standard graphics device.
##' @return invisibly returns a list with 2 elements:
##' ggplot: the ggplot object to be plotted (this can be added to
##' dat: a named list of the data frame(s) passed to data in ggplot.  The names come from converting the rows argument to a character vector.
##' @import ggplot2
##' @importFrom grid unit
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @seealso \code{\link{ggplot2}}, \code{\link{qplot}}
##' @export
##' @examples
##' data(OrchardSprays)
##' ## Example of functionality
##' ggSmartBoxplot(x=OrchardSprays$treatment,
##'                mat=t(OrchardSprays[,1]),
##'               rows=1, whichCols=1:ncol(t(OrchardSprays)),
##'               colorBy=factor(OrchardSprays$rowpos+OrchardSprays$colpos > 9),
##'               xlab="Treatment")
##'
##' \dontrun{
##' cellType <- "PBMC"
##' ## expr would be an expression matrix with genes in rows and samples in columns 
##' geneSub <- grep("HLA-A29.1", rownames(expr))
##' age <- "Young"
##' ages <- c("Young", "Old")
##' responses <- c("NR", "R")
##' subset <- targetFClist[[cellType]]$Age %in% ages & 
##'   targetFClist[[cellType]]$Response %in% responses
##' ggSmartBoxplot(x=targetFClist[[cellType]][subset, "Time"],
##'                mat=exprFClist[[cellType]], ylim=c(-1,1),
##'                rows=geneSub, whichCols=which(subset), 
##'                colorBy=targetFClist[[cellType]][subset,"Response"],
##'                splitRowBy=targetFClist[[cellType]][subset,"Age"],
##'                xlab="Days (Post Vaccination)",
##'                fileName=NA)
##' }
ggSmartBoxplot <- function(x, mat, splitRowBy=NA, splitColBy=NA, colorBy=NULL, rows, cols=NA,
                           whichCols=NA, sep='.', outlier.shape=17, outlier.color=NULL,
                           ylab=NULL, space="fixed", scales="fixed",                           
                           fileName=NA, plot=TRUE, ...)  {
#  require(ggplot2)
  if(is.character(fileName))
    pdf(fileName)
  datList <- vector(mode="list", length=length(rows))
  names(datList) <- as.character(rows)
  lcv <- 1
  for(r in rows) {
    if(is.na(cols) && !is.na(whichCols)) {
      submat <- mat[r,whichCols]
      ## Find the common string separated  sep in the column names
    } else if (is.na(whichCols) && !is.na(cols)) {
      submat <- mat[r,grep(paste0(sep, cols, sep), colnames(mat), fixed=T)]
    } else
      stop("Must specify columns in cols or whichCols argument")
    ## x2 is defined
    x2 <- as.numeric(as.factor(x))
    if(!is.null(colorBy)) {
      x2 <- x2 + 
        0.25*seq(from=-1, to=1,
                 length.out=length(levels(factor(colorBy))))[as.numeric(factor(colorBy))]
    }
    dat <- data.frame(x = factor(x), x2 = x2, vals = unlist(submat),
                      splitColBy = splitColBy, splitRowBy = splitRowBy)
    if(!is.null(colorBy)) {
      dat$colorBy <- factor(colorBy)
      cbLoop <- levels(dat$colorBy)
    } else {
      cbLoop <- NA
    }
    ## print(dat)
    ## Assign rownames as r or the rownames at row number r
    if(is.numeric(r)) {
      rowName <- rownames(mat)[r]
    } else {
      rowName <- r
    }
    ## Determine the outliers in each of the subsets
    dat$is.outlier <- rep(FALSE, nrow(dat))
    for(ex in unique(x)) {
      xTmp <- (dat$x == ex)
      for(sc in unique(dat$splitColBy)) {
        if(is.na(sc)) {scTmp <- rep(TRUE, nrow(dat))} else {scTmp <- dat$splitColBy == sc}
        for(sr in unique(dat$splitRowBy)) {      
          if(is.na(sr)) {srTmp <- rep(TRUE, nrow(dat))} else {srTmp <- dat$splitRowBy == sr}
          for(cb in cbLoop) {
            if(is.na(cb)) {cbTmp <- rep(TRUE, nrow(dat))} else {cbTmp <- dat$colorBy == cb}
            sel <- (scTmp) & (srTmp) & (cbTmp) & (xTmp)
            y <- dat[sel,"vals"]
            lowerq = quantile(y)[2] # 25%
            upperq = quantile(y)[4] # 75%
            iqr = upperq - lowerq   # Or use IQR(data)
            default.thresh.upper = (iqr * 1.5) + upperq
            default.thresh.lower = lowerq - (iqr * 1.5)
            ind <- which( (y < default.thresh.lower) | (y > default.thresh.upper) )
            if(length(ind) > 0) {
              dat$is.outlier[which(sel)[ind]] <- TRUE
            }
          }
        }
      }
    }
    datList[[lcv]] <- dat
    ## print(dat)    
#f <- ggplot(dat, aes(x=x, y=vals)) +                                       
#        scale_x_discrete() +
#          geom_boxplot(aes(fill=colorBy), color=c("yellow", "blue"), outlier.shape=NA,
#                       position=position_dodge(1)) +
    if(is.null(ylab)) {ylab <- rowName}
    f <- qplot(x, vals, data = dat, geom="boxplot", ylab=ylab, fill=colorBy,
               position=position_dodge(1),
               outlier.shape=outlier.shape,
               outlier.color=outlier.color,
               ...) +
                 geom_jitter(data=subset(dat, !is.outlier),
                             aes(x=x2),
                             position=position_jitter(width=0.1)) +
                   theme_bw() +
                     theme(plot.margin=unit(c(5, 3, 5, 10), "mm"),
                           text=element_text(size=16),
                           axis.title.x = element_text(vjust=-.75),
                           axis.title.y = element_text(vjust=1.00))
    ## Default colors for only 2 classes are yellow and blue
    if(length(levels(colorBy)) == 2)
      f <- f + scale_fill_manual(name="", breaks=factor(colorBy), values=c("yellow", "blue"))
    ## Determine how to split based on values of splitRowBy and splitColBy
    if (!all(is.na(splitColBy)) && all(is.na(splitRowBy))) {
      f <- f + facet_grid(. ~ splitColBy, scales = scales,
                          space = space)
    } else if (!all(is.na(splitRowBy)) && all(is.na(splitColBy))) {
      f <- f + facet_grid(splitRowBy ~ ., scales = scales,
                          space = space)
    } else if (!all(is.na(splitRowBy)) && !all(is.na(splitColBy))) {
      f <- f + facet_grid(splitRowBy ~ splitColBy, scales = scales,
                          space = space)
    }
    ## No legend if only 1 color is used - does not work
    ## if(length(unique(colorBy)) == 1) {
    ##   f <- f + guides(color=FALSE)
    ## }
    if(plot) {
      plot(f)
    }
    lcv <- lcv + 1
  }
  if(is.character(fileName))
    dev.off()
  return(invisible(list(ggplot=f, dat=datList)))
}


