##' ggSmoothExprPlot
##'
##' Wrapper around ggplot to transform data and plot profiles (e.g. expression or activity) over time
##'
##' @param x the numeric x-axis variable for the plot (usually time)
##' @param mat data.frame or matrix of values to plot with samples in columns
##' @param rows row names or row indices of the items to be plotted
##' @param method smoothing method (function). See \code{stat_smooth}
##' @param formula a formula to use for smoothing in \code{stat_smooth} (e.g. the default "y ~ x" or "y ~ ns(x, 3)").
##' @param splitRowBy a factor used to split the data by row in facet_grid
##' @param splitColBy a factor used to split the data by col in facet_grid
##' @param colorBy a factor used for coloring. No coloring will be done if \code{NULL} (default)
##' @param cols substring to search for with "grep" in column names to be plotted
##' @param whichCols the column indices or full column names
##' @param sep a separator used in searching for cols in the column names
##' @param colorByLabel the labels used for the color legend
##' @param ggtitle logical. If TRUE, \code{rows} is coerced to character and passed to ggtitle
##' @param xlab passed to \code{xlab}. Defaults to "Time (Post-Vaccination)"
##' @param ylab passed to \code{ylab}. Defaults to "Expression"
##' @param colors The colors to use. Defaults to the colors given by using RColorBrewer's "Dark2" pallette (but RColorBrewer is not called directly so is not required).
##' @param scales Are scales shared across all facets (the default, \code{"fixed"}), or do they vary across rows (\code{"free_x"}), columns (\code{"free_y"}), or both rows and columns (\code{"free"})
##' @param fileName the name of a file to write a PDF to or \code{NA} to plot in standard graphics device.
##' @param plot logical specifying whether or not to plot the plot(s). Default is TRUE.
##' @param space If \code{"fixed"}, the default, all panels have the same size.  If \code{"free_y"} their height will be proportional to the length of the y scale; if \code{"free_x"} their width will be proportional to the length of the x scale; or if \code{"free"} both height and width will vary.  This setting has no effect unless the appropriate scales also vary.
##' @return invisibly returns a list with 2 elements:
##' ggplot: the ggplot object to be plotted (this can be added to
##' dat: a named list of the data frame(s) passed to data in ggplot.  The names come from converting the rows argument to a character vector.
##' @import ggplot2
##' @author Stefan Avey
##' @keywords aveytoolkit
##' @seealso \code{\link{ggplot2}}
##' @export 
##' @examples
##' data(OrchardSprays)
##' ## Example of functionality
##' library(Biobase)
##' data(sample.ExpressionSet, package="Biobase")
##' dat <- sample.ExpressionSet
##' ## Normally x-axis is time but in this dataset there is no time
##' ## so we will use the `score` as the x-axis
##' genderF <- dat$sex == "Female"
##' ggSmoothExprPlot(x=dat$score[genderF],
##'                  mat=exprs(dat),
##'                  rows="31345_at",
##'                  whichCols=which(genderF), # females only
##'                  colorBy=as.factor(dat$type)[genderF],
##'                  colorByLabel="Condition",
##'                  xlab="score")
##' \dontrun{tmp <- ggSmoothExprPlot(x=times[subset], mat=expr, rows=gene,
##'                                  formula=formula("y ~ ns(x,3)"),
##'                                  whichCols=subset, colorBy=target[subset,respType],
##'                                  splitColBy=splitby,
##'                                  splitRowBy=as.factor(target[subset,"Study"]),
##'                                  ggtitle=TRUE, colorByLabel=respType, plot=TRUE)}
ggSmoothExprPlot <- function(x, mat, rows, method="auto",
                             formula=formula("y ~ x"),
                             splitRowBy=NA, splitColBy=NA,
                             colorBy=NULL, cols=NA, whichCols=NA, sep='.',
                             colorByLabel="Response", ggtitle=TRUE,
                             xlab="Time (Post-Vaccination)", ylab="Expression",
                             colors=c("#1B9E77", "#D95F02", "#7570B3", "#E7298A",
                               "#66A61E", "#E6AB02", "#A6761D", "#666666"),
                             space="fixed", scales="fixed",
                             fileName=NA, plot=TRUE)  {
  if(is.character(fileName))
    pdf(fileName)
  if(!is.numeric(x)) {
    stop("'x' must be numeric.")
  }
  datList <- vector(mode="list", length=length(rows))
  names(datList) <- as.character(rows)
  lcv <- 1
  for(r in rows) {
    if(is.na(cols) && !is.na(whichCols)) {
      submat <- mat[r,whichCols]
      ## Find the common string separated by `sep` in the column names
    } else if (is.na(whichCols) && !is.na(cols)) {
      submat <- mat[r,grep(paste0(sep, cols, sep), colnames(mat), fixed=T)]
    } else
      stop("Must specify columns in cols or whichCols argument")
    dat <- data.frame(x = as.numeric(x), vals = unlist(submat),
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
    f <- ggplot(data=dat) +
      stat_smooth(alpha=0.1, size=1.5, aes(x=x, y=vals, fill=colorBy, color=colorBy),
                  method=method, formula = formula) +
                    geom_jitter(data=dat, aes(x=x, y=vals, color=colorBy),
                                position=position_jitter(width=0.2))
    f <- f + 
        scale_fill_manual(name=colorByLabel, breaks = levels(dat$colorBy), values = colors) +
          scale_color_manual(name=colorByLabel, breaks = levels(dat$colorBy), values = colors) +
            xlab(xlab) + 
              ylab(ylab) +
                theme_bw()
    if(ggtitle) {
      f <- f + ggtitle(r)
    }
    ## theme(plot.margin=unit(c(5, 3, 5, 10), "mm"),
    ##       text=element_text(size=16),
    ##       axis.title.x = element_text(vjust=-.75),
    ##       axis.title.y = element_text(vjust=1.00))
    ## Determine how to split based on values of splitRowBy and splitColBy
    if (!all(is.na(splitColBy)) && all(is.na(splitRowBy))) {
      f <- f + facet_grid(. ~ splitColBy, scales = scales, space = space)
    } else if (!all(is.na(splitRowBy)) && all(is.na(splitColBy))) {
      f <- f + facet_grid(splitRowBy ~ ., scales = scales, space = space)
    } else if (!all(is.na(splitRowBy)) && !all(is.na(splitColBy))) {
      f <- f + facet_grid(splitRowBy ~ splitColBy, scales = scales, space = space)
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


