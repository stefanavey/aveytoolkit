% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aveytoolkit_PlotTimeCourse.R
\name{PlotTimeCourse}
\alias{PlotTimeCourse}
\title{PlotTimeCourse}
\usage{
PlotTimeCourse(
  x,
  y,
  colors,
  groups,
  sampleNames,
  pch = 19,
  plotTitle = "",
  legend.loc = "topleft",
  plotType = c("times", "points"),
  alpha = 0.15,
  cex.pt = 1,
  cex.time = 2,
  time.adj = c(-0.3, -0.3),
  arrLen = 0.1,
  lwd = 3,
  numRep = 3,
  plotFont = NULL,
  ctrl = TRUE,
  hourMarks = TRUE,
  legend.cex = 2,
  ...
)
}
\arguments{
\item{x}{x-values for plotting}

\item{y}{y-values for plotting}

\item{colors}{named vector specifying colors for each sample}

\item{groups}{the virus strain names for the conditions of interest}

\item{sampleNames}{names of the samples}

\item{pch}{the plotting character. Default is 19 (a closed circle).}

\item{plotTitle}{a string used for the plotting title}

\item{legend.loc}{location of the legend. Default is topleft.}

\item{plotType}{one of "times" or "points".  See Details}

\item{alpha}{transparency factor passed to the alpha function (scales library)}

\item{cex.pt}{size of points. Default is 1}

\item{cex.time}{size of time labels. Default is 2}

\item{time.adj}{the ammount to adjust the time labels.  Default is c(-.3, -.3) which moves them to the lower left}

\item{arrLen}{length of the arrows plotted at the average of each time point. Default is 0.1}

\item{lwd}{line width. Default is 3}

\item{numRep}{the number of replicates. Default is 3}

\item{plotFont}{which font to use for plotting text}

\item{ctrl}{should the control time points be included?}

\item{hourMarks}{should the 4 and 8 hour time points be marked on the plot?}

\item{legend.cex}{size expansion for the legened. Default is 2.}

\item{...}{other arguments passed to heatmap.2}
}
\value{
Nothing is returned
}
\description{
Plot helper function for PlotPCATimeCourse
}
\details{
If plotType is "times", ??? Also used to plot 2 genes expression against each other over time. If legend.loc is "none", no legend is plotted. ctrl flag indicates whether or not first numRep values in x and y are from a control measurement
}
\note{
Colors are assumed to have as the names attribute some part of the sampleName which can uniquely identify it.
}
\author{
Stefan Avey
}
\keyword{aveytoolkit}
