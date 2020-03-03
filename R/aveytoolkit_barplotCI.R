##' barplotCI
##'
##' Create a barplot from the list x with one bar for each element of x
##'
##' @param x list of values to create a boxplot from
##' @param CIs the confidence intervals. Default is \code{NULL} and they will be calculated as the 95\% confidence interval.
##' @param compareTo non-negative integer specifying to which element of x should comparisons be made for significance. If 0, no significance will be added.
##' @param ... other arguments passed to \code{barplot2} function
##'
##' @return Same as return from barplot2.
##' A numeric vector (or matrix, when \code{beside = TRUE}), say \code{mp}, giving the coordinates of _all_ the bar midpoints drawn, useful for adding to the graph.
##' If \code{beside} is true, use \code{colMeans(mp)} for the midpoints of each _group_ of bars, see example.
##'
##' @author Christopher Bolen (creator) ; Stefan Avey (modified)
##' @export 
barplotCI <- function(x, CIs=NULL, compareTo=1, ...) {
  ## require(gplots)
  lx <- length(x)
  means = sapply(x,function(i){if(is.matrix(i)){rowMeans(i)}else{mean(i)}})
  if(is.matrix(means)){means=t(means)}else{means=as.matrix(means)}
  
  if(is.null(CIs)){
    sds = sapply(x,function(i){
      if(is.matrix(i)){
        return(apply(i,1,sd))
      }else{return(sd(i))}
    })
    if(is.matrix(sds)){sds=t(sds)}else{sds=as.matrix(sds)}
    
    len = sapply(x,function(i){if(is.matrix(i)){ncol(i)}else{length(i)}})
    CIs = qt(0.975,len-1)/sqrt(len)*sds
  }
  ci.u <- means+CIs
  ci.l <- means-CIs
  ## Barplot
  xloc = gplots::barplot2(means,plot.ci=TRUE,ci.u=ci.u,ci.l=ci.l,beside=TRUE, ci.lwd=2, ...)
  ## Add significance
  if( (compareTo > 0) & (compareTo <= lx) ){
    ps = as.matrix(sapply(1:lx,function(i){
      if(is.matrix(x[[i]])){
        return(sapply(1:nrow(x[[i]]),function(j){
          if(i==compareTo){return(NA)}
          temp = try(t.test(x[[compareTo]][j,],x[[i]][j,])$p.value, silent=T)
          if(class(temp)=="try-error"){return(NA)}
          return(temp)
        }))
      }else{
        if(i==compareTo){return(NA)}
        return(t.test(x[[compareTo]],x[[i]])$p.value)
      }
    }))
    p.text = as.matrix(apply(ps, 1, function(p){
      c("***","**","*","")[findInterval(p, c(0,0.001,0.01,0.05,1))]
    }))
  } else if(compareTo > lx) {
    stop("compareTo must be from 1 to length(x)")
  } else {
    p.text <- matrix(NA, nrow=lx, ncol=ncol(xloc))
  }
  for(i in (1:lx)){
    if(i==compareTo){next;}
    for(j in 1:ncol(xloc)){
      if(!is.na(p.text[i,j]) && p.text[i,j]!=""){
        yloc = max((means+CIs)[c(i,compareTo),j])+strheight("*")*(rev(cumsum(rev(p.text[,j]!="")))[i]*2)
        ## print(x.cord)
        ## cat('\n\n')
        ## print(y.cord)
        ## Draw the lines between bars
        arrows(xloc[i,j],yloc,xloc[compareTo,j],yloc,code=3,angle=90,
               length=strheight("*",units="inches") /2)
        text(mean(xloc[c(i,compareTo),j]),yloc,p.text[i,j],adj=c(0.5,-1.2))
      }
    }
  }
  return(invisible(xloc))
}
