#' runLimma
#'
#' This function performs a basic LIMMA analysis on the given expression set
#'
#' @param eset the expression matrix
#' @param labels the labels for each column of the eset
#' @param contrasts Vector of contrasts to make
#' @param block Vector of factors specifying a blocking variable (i.e. for paired samples or for ). NOT IMPLEMENTED!
#' @param covariates data frame of covariates (of same length as labels) to include in the model. Use this if there are paired samples, etc.
#' @param filterReplicateGenes Only include one probeset for each gene (determined by symbol)
#' @param min.fold.change Minimum log2 fold change to be differentially expressed. Default is 1.
#' @param min.intensity Minimum log2 intensity (at any time) to be differentially expressed. Default is 4.
#' @param p.cutoff FDR corrected cutoff for significant differential expression. Default is 0.05.
#' @param fitOnly If true, will return fit2, rather than the matrix of significant genes. Default is FALSE.
#' @details Generally, an expression matrix is made up of rows of genes (or any other features) and columns of samples. The matrix has data for multiple classes (which are denoted with the 'labels' parameter) and the classes are compared using the vector of contrasts.
#' @import limma
#' @author Christopher Bolen
#' @keywords aveytoolkit
#' @seealso \code{\link{limma}}
#' @export
#' @examples
#' ## Example:
#' ## If you have a m X 10 matrix 'eset', with 5 samples of class A and 5 of class B,
#' ## you could compare class A to class B using the following code:
#' ##   
#' ## results = runLimma(eset, c('A','A','A','A','A','B','B','B','B','B'), "B-A")
#' ## 
#' ## This will return to you a matrix with columns for each comparison and rows for each gene.
#' ## The value in each cells will either be -1, 0, or 1, depending on whether the gene is
#' ## significantly higher in B, not significant, or significantly higher in A, respectively. 
#' ## If you want information on p-values and fold changes, set "fitOnly=T", and you can access
#' ## the fit object to get the information.
#' ## 
#' ## For other comparisons, you can look at the LIMMA user guide.
runLimma  <- function(eset,
                      labels,                           
                      contrasts,                        
                      ## block = NULL,                  
                      covariates=NULL,                  
                      filterReplicateGenes = TRUE,      
                      min.fold.change = 1,              
                      min.intensity = 4,                
                      p.cutoff = 0.05,                  
                      fitOnly = FALSE)                   
{                                                                      
  ##trim eset
  tooLow =  eset < min.intensity
  drop = apply(tooLow, 1, all)
  eset = eset[!drop,]
  
  labels = as.factor(as.vector(labels))
  if(is.null(covariates)){
    design <- model.matrix(~ -1+labels)
    colnames(design) <- levels(labels)
  }else{
   if(!is.data.frame(covariates)){stop("Covariates must be a data frame")}
   
   f = paste(c("~ 0 + labels",colnames(covariates)),collapse="+")
   cns = levels(labels) #colnames for the final design matrix
   for(i in 1:ncol(covariates)){
     covariates[,i] = as.factor(covariates[,i])
     cns = c(cns, paste("cov",i,levels(covariates[,i])[-1],sep="."))
   }
   attach(covariates, warn.conflicts=F)
   design <- model.matrix(formula(f))
   colnames(design) = cns
   detach(covariates)
  }
  #cor = NA
  #if(!is.null(block)){
  #  block = as.factor(block)
  #  corfit = duplicateCorrelation(eset, design=design, block=block)
  #  cor = corfit$consensus
  #}
  
  fit <- lmFit(eset, design=design)#, cor=cor, block=block)
  rownames(fit$coefficients) <- rownames(eset)             ## For some unknown reason, using block causes rownames to disappear
  rownames(fit$stdev.unscaled) <- rownames(eset)           ## For some unknown reason, using block causes rownames to disappear
  
  contrast.matrix <- makeContrasts( contrasts=contrasts, levels=design)
  fit2 <- contrasts.fit(fit,contrast.matrix)
  fit2 <- eBayes(fit2) 
  results <- decideTests(fit2,adjust.method="BH",p.value=p.cutoff,lfc=min.fold.change)
  
  if(fitOnly){
    return(fit2)
  }else{
    return(results) 
  }
  
}

## My own homebrewed version of the Volcano Plot that limma has.
## If you run Limma with "fitOnly=T", you can simply stick the fit object from the function
## into this one and it will produce a volcano plot from the first comparison that you ran.
## To change what comparison you make the plot for, set "coef" to the number of the comparison.
volcanoPlot = function(fit, 
                       coef=1,
                       colored=T,
                       min.lfc=1,
                       p.cutoff=0.05,
                       convertToFDR=T,
                       xlab="Log Fold Change",
                       ylab=NA,
                       pch=16,
                       cex=0.35,
                       ...
                       ){
  lfc = fit$coefficients[,coef]
  pval = fit$p.value[,coef]
  if(convertToFDR){
    pval = p.adjust(pval, "fdr")
    if(is.na(ylab)){ylab="-log10(False Discovery Rate)"}
  }
  if(is.na(ylab)){ylab="-log10(P Value)"}
  
  
  if(!is.na(p.cutoff) & !is.na(min.lfc)){
    cols = c("grey","dark grey","red")[(abs(lfc)>min.lfc) + (pval<p.cutoff) + 1]
  }else{cols="black"}
  plot(lfc, -log10(pval), col=cols, xlab=xlab,ylab=ylab,pch=pch,cex=cex,...)
  abline(h=-log10(p.cutoff))
  abline(v=min.lfc)
  abline(v=-min.lfc)
}
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


