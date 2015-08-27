##' runLimma
##'
##' A wrapper around limma functions to perform a basic analysis on the given expression matrix
##'
##' @param eset the expression matrix (not expression set object)
##' @param labels the labels for each column of the eset
##' @param contrasts Vector of contrasts to make
##' @param block vector or factor specifying a blocking variable on the arrays. Has length equal to the number of arrays. Must be ‘NULL’ if ‘ndups>2’. (Not extensively tested, use with caution)
##' @param covariates data frame of covariates (of same length as labels) to include in the model. Use this if there are paired samples, etc.
##' @param min.fold.change Minimum log2 fold change to be differentially expressed. Default is 1.
##' @param min.intensity Minimum log2 intensity (at any time) to be differentially expressed. Default is 4.
##' @param p.cutoff FDR corrected cutoff for significant differential expression. Default is 0.05.
##' @param fitOnly If true, will return fit2, rather than the matrix of significant genes. Default is FALSE.
##' @return depends on \code{fitOnly}
##' @details Generally, an expression matrix is made up of rows of genes (or any other features) and columns of samples. The matrix has data for multiple classes (which are denoted with the 'labels' parameter) and the classes are compared using the vector of contrasts. Block can be used for biological (or technical) replicates or for separate subjects (in which case it will determinte the inter-subject correlation).  See \code{?duplicateCorrelation} for more information.
##' ## Example:
##' If you have a m X 10 matrix 'eset', with 5 samples of class A and 5 of class B,
##' you could compare class A to class B using the following code:
##'   
##' results = runLimma(eset, c('A','A','A','A','A','B','B','B','B','B'), "B-A")
##' 
##' This will return to you a matrix with columns for each comparison and rows for each gene.
##' The value in each cells will either be -1, 0, or 1, depending on whether the gene is
##' significantly higher in B, not significant, or significantly higher in A, respectively. 
##' If you want information on p-values and fold changes, set "fitOnly=T", and you can access
##' the fit object to get the information.
##' 
##' For other comparisons, you can look at the LIMMA user guide: limmaUsersGuide()
##' @import limma
##' @author Christopher Bolen, Stefan Avey
##' @keywords aveytoolkit
##' @seealso \code{\link{limma}}
##' @export
##' @examples
##' \dontrun{
##' ## Load in example data from colonCA package (install if necessary)
##' ## source("http://bioconductor.org/biocLite.R")
##' ## biocLite("colonCA")
##' library(colonCA)
##' ## Look at head of data
##' head(pData(colonCA))
##' labels <- pData(colonCA)$class          # t and n for tumor and normal
##' ## Data are paired (-1 and 1 come from same subject)
##' pair <- factor(abs(pData(colonCA)$samp))
##' covars <- data.frame(Pairing=as.character(pair))
##' deRes <- runLimma(eset=exprs(colonCA), labels=as.character(labels), contrasts="t-n",
##'                   covariates=covars, fitOnly=TRUE)
##' topTable(deRes)
##' ## Or just do tests in the function to get -1, 0, 1 for DE status of each probe
##' testRes <- runLimma(eset=exprs(colonCA), labels=as.character(labels), contrasts="t-n",
##'                     covariates=data.frame(Pairing=as.character(pair)), fitOnly=FALSE)
##' head(testRes)
##' }
runLimma  <- function(eset, labels, contrasts, block = NULL, covariates=NULL,
                      min.fold.change = 1, min.intensity = 4, p.cutoff = 0.05, fitOnly = FALSE)
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
   attach(covariates, warn.conflicts=FALSE)
   design <- model.matrix(formula(f))
   colnames(design) = cns
   detach(covariates)
  }
  cor <- NULL
  if(!is.null(block)){
   block <- as.factor(block)
   corfit <- duplicateCorrelation(eset, design=design, block=block)
   cor <- corfit$consensus.correlation
  }
  fit <- lmFit(eset, design=design, block=block, correlation=cor)
  ## fit <- lmFit(eset, design=design)#, cor=cor, block=block)  
  ## For some unknown reason, using block causes rownames to disappear
  rownames(fit$coefficients) <- rownames(eset) 
  rownames(fit$stdev.unscaled) <- rownames(eset)           
  
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
