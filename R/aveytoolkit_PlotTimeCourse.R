#' PlotTimeCourse
#'
#' Plot helper function for PlotPCATimeCourse
#'
#' @param x x-values for plotting
#' @param y y-values for plotting
#' @param colors named vector specifying colors for each sample
#' @param groups the virus strain names for the conditions of interest
#' @param sampleNames names of the samples
#' @param pch the plotting character. Default is 19 (a closed circle).
#' @param plotTitle a string used for the plotting title
#' @param legend.loc location of the legend. Default is topleft.
#' @param plotType one of "times" or "points".  See Details
#' @param alpha transparency factor passed to the alpha function (scales library)
#' @param cex.pt size of points. Default is 1
#' @param cex.time size of time labels. Default is 2
#' @param time.adj the ammount to adjust the time labels.  Default is c(-.3, -.3) which moves them to the lower left
#' @param arrLen length of the arrows plotted at the average of each time point. Default is 0.1
#' @param lwd line width. Default is 3
#' @param numRep the number of replicates. Default is 3
#' @param plotFont which font to use for plotting text
#' @param ctrl should the control time points be included?
#' @param hourMarks should the 4 and 8 hour time points be marked on the plot?
#' @param legend.cex size expansion for the legened. Default is 2.
#' @param ... other arguments passed to heatmap.2
#' @return Nothing is returned
#' @details If plotType is "times", ??? Also used to plot 2 genes expression against each other over time. If legend.loc is "none", no legend is plotted. ctrl flag indicates whether or not first numRep values in x and y are from a control measurement
#' @note Colors are assumed to have as the names attribute some part of the sampleName which can uniquely identify it.
#' @author Stefan Avey
#' @keywords aveytoolkit
#' @export

PlotTimeCourse <- function(x, y, colors, groups, sampleNames, pch=19, plotTitle="", 
                           legend.loc="topleft", plotType=c("times", "points"),
                           alpha=0.15, cex.pt=1, cex.time=2, time.adj=c(-.3,-.3),
                           arrLen=.1, lwd=3, numRep=3, plotFont=NULL, ctrl=TRUE,
                           hourMarks=TRUE, legend.cex=2, ...)
{
  plot(x, y, type="n", main=plotTitle, ...)
  plotType <- match.arg(plotType)
  switch(plotType,
         times = text(x, y, labels=pch, col=colors[groups], cex=cex.pt, font=plotFont),
         points = {  require(scales)
                     if(numRep > 1) { # only if there are points not on lines
                       if(ctrl) # if first time points are control
                         cols <- c(colors[groups[1:numRep]], alpha(colors[groups[-(1:numRep)]], alpha))
                       else
                         cols <- alpha(colors[groups], alpha)
                       if(is.numeric(pch)) # probably want to plot symbols if numeric
                         points(x, y, pch=pch, col=cols, cex=cex.pt)
                       else
                         text(x, y, labels=pch, col=cols, cex=cex.pt, font=plotFont)
                     }
                   }
         )
  for(i in 1:length(colors)) { # Loop through conditions
    whichCols <- grep(names(colors)[i], sampleNames, fixed=T)
    x.avg <- rowMeans(matrix(x[whichCols], ncol=numRep, byrow=TRUE))
    y.avg <- rowMeans(matrix(y[whichCols], ncol=numRep, byrow=TRUE))
    if(length(whichCols) > numRep) { # if there is more than one time point
      if(hourMarks) {
        if(names(colors)[i] == "AlaF") # AlaF only has 7 time points so change whichVals
          whichVals <- c(4,7)
        else
          whichVals <- c(4,8)
        text(x.avg[whichVals], y.avg[whichVals], labels=c("4", "8"), col=colors[i],
             adj=time.adj, cex=cex.time, font=plotFont)
      }
      if(ctrl) { # if Control time point is included
        conSamp <- grep("Control", sampleNames)
        arrows(mean(x[conSamp]), mean(y[conSamp]), x.avg[1], y.avg[1], col=colors[i], length=arrLen,
               lwd=lwd)
      }
      for(j in 1:(length(x.avg)-1))
        arrows(x.avg[j], y.avg[j], x.avg[j+1], y.avg[j+1], col=colors[i], length=arrLen,
               lwd=lwd)
    }
    ## lines(x.avg, y.avg, col=colors[i])
  }
  legNames <- names(colors)
  funcStrains <- c("Brevig", "Cal", "NC", "Tx")
  names(funcStrains) <- c("1918", "Cal/09", "NC", "TX") # 'official names'
  legNames[match(funcStrains, names(colors), nomatch=0)] <- names(funcStrains)[match(names(colors), funcStrains, nomatch=0)]
  if(legend.loc != "none")
    legend(legend.loc, legNames, fill=colors, cex=legend.cex)
}

