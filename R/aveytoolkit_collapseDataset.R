##' collapseDataset
##'
##' Collapses a dataset from probes to gene symbols.
##'
##' @param exprsVals a matrix or data.frame of numeric values with rownames denoting the identifiers.
##' @param platform the microarray platform the data comes from for extracting the gene symbols
##' @param mapVector a named character vector with names specififying the current identifiers (probes matching the rownames of exprsVals) and the values of the vector specifying the gene symbols (or other identifier to collapse to). 
##' @param oper the operation used to choose which probe when multiple probes map to the same gene.  Default is max which will calculate the maximum of the average.
##' @param prefer one of "none", "up", or "down", can be abbreviated.
##' @param singleProbeset If \code{TRUE}, the operation applies to the average over all conditions and all values for a gene will come from one probeset. Otherwise, if \code{FALSE}, the operation applies to the probesets over all conditions and the values for a gene may come from different probe sets . Default is \code{FALSE} for compatability reasons but \code{TRUE} is recommended.
##' @param returnProbes if \code{TRUE}, a list of the collapsed expression matrix and the probes are both returned (see return).
##' @param deProbes a list with named vectors "up" and "down" giving the names of up and downregulated probes
##' @param debug When TRUE, things will be printed out to help debug errors
##' @return If returnProbes is \code{TRUE}, a list containing the collapsed dataset in $exprsVals and the probes chosen in $probeSets.  Otherwise, if returnProbes is \code{FALSE}, only the expression matrix is returned.
##' @details This function is designed to work for microarray data but can work for any sort of numeric matrix for which multiple rows need to be collapsed.
##'
##' If singleProbeset is set to \code{FALSE}, the default for compatability reasons but untested and not recommended, the values for each sample will be taken from the maximum across any probe that maps to that gene.  This means that a gene's expression values may be a composition of values from different probes rather than a single probe.   Most users will not need to use the `prefer` argument.  If prefer is "up", when multiple deProbes match the same gene, the upregulated will be chosen.  Similary for "down".  Default is "none" and the probe with the `oper` (default max) will be chosen.
##'
##' Note that it is possible for multiple probes to have the same operation (\code{oper}) over all conditions and, in this case, I've decided arbitarily to choose the first one.
##' @author Christopher Bolen, Modified by Stefan Avey
##' @keywords aveytoolkit
##' @export
##' @examples
##' ## Trivial Example showing basic functionality
##' fakeExpr <- matrix(rnorm(50, mean=8, sd=1), ncol=5, nrow=10,
##'                    dimnames=list(probes=paste("probe", 1:10, sep='_'),
##'                      samples=paste("sample", LETTERS[1:5], sep='_')))
##' mv <- rep(paste("Gene", LETTERS[1:5], sep='_'), each=2) # mapVector
##' names(mv) <- rownames(fakeExpr)
##' res <- collapseDataset(fakeExpr, mapVector=mv, oper=max,
##'                        singleProbeset=TRUE, # recommend setting singleProbeset to TRUE
##'                        returnProbes=TRUE) 
##' res$probes
##' ## between probe_1 and probe_2, probe_2 was chosen for Gene_A
##' ## between probe_3 and probe_4, probe_4 was chosen for Gene_B
##' ## etc.
##' 
##' res$exprsVals                           # collapsed expression values
##' 
##' ## only difference is in rownames, numbers are identical
##' all.equal(res$exprsVals, fakeExpr[res$probes,])
collapseDataset <- function(exprsVals, platform=NULL, mapVector=NULL, oper = max,
                            prefer=c("none", "up", "down"), singleProbeset=FALSE, returnProbes=FALSE,
                            deProbes=NULL, debug=FALSE)
{
  if(is.null(platform) && is.null(mapVector)){
    stop("Need to include either a platform or a named vector to map to")
  }                              
  if(!is.null(platform)){
                                        #find the right columns
    if("Gene.Symbol" %in% names(platform)){ mapVector = as.vector(platform$Gene.Symbol)}
    else if("GeneSymbol" %in% names(platform)){ mapVector = as.vector(platform$GeneSymbol)}
    else if("SYMBOL" %in% names(platform)){ mapVector = as.vector(platform$SYMBOL)} 
    else if("GENE_SYMBOL" %in% names(platform)){ mapVector = as.vector(platform$GENE_SYMBOL)} 
    else if("Symbol" %in% names(platform)){ mapVector = as.vector(platform$Symbol)}
    else{stop("Gene Symbol column cannot be found in the platform")}
    names(mapVector) = toupper(as.vector(platform$ID))             ##wonderful! no consistency even for upper/lower case probes
  }else if(!is.null(mapVector)){names(mapVector) = toupper(names(mapVector))}
  if(class(mapVector)=="list"){mapVector=unlist(mapVector)}
  prefer <- match.arg(prefer)
  
  probeSets = toupper(rownames(exprsVals))
  geneSymbols = mapVector[probeSets]
  allDEprobes <- Reduce(union, deProbes)
  
                                        #remove probes with NO gene symbol
  probeSets_NoGeneSymbol = ((geneSymbols == "") | is.na(geneSymbols))
  exprsVals = as.matrix(exprsVals[ !probeSets_NoGeneSymbol,])
  probeSets = probeSets[!probeSets_NoGeneSymbol]
  geneSymbols = geneSymbols[!probeSets_NoGeneSymbol]
  
                                        #remove rows that contain NAs
  missingData = apply(is.na(exprsVals), 1,any)
  exprsVals = as.matrix(exprsVals[ !missingData,])
  probeSets = probeSets[!missingData]
  geneSymbols = geneSymbols[!missingData]

  ## browser() ## debugging
  
  ## combine all the probes for genes with more than one name using the following function
  geneSymbols_Multiple =  names(which(table(geneSymbols)>1))
  maxOfProbes = matrix(NA,nrow=length(geneSymbols_Multiple),ncol=dim(exprsVals)[2],
      dimnames=list(geneSymbols_Multiple,colnames(exprsVals)))
  maxProbeNames <- rep(NA, length(geneSymbols_Multiple))
  names(maxProbeNames) <- geneSymbols_Multiple
  for(g in geneSymbols_Multiple){
    if(debug) { cat("Gene: ", g, '\n') }
    probes <- as.matrix(exprsVals[ which( geneSymbols %in% g ), ])
    if(!is.null(deProbes)) {
      ## If any probes are DE, choose the maximum from among those ones
      matchingProbes <- names(mapVector[mapVector == g])
      matchingProbes.de <- matchingProbes[which(matchingProbes %in% allDEprobes)]
      if(length(matchingProbes.de) > 0) {
        probes <- matrix(exprsVals[ (geneSymbols %in% g) &
                                       (probeSets %in% matchingProbes.de), ],
                         nrow=length(matchingProbes.de), ncol=dim(exprsVals)[2])
        rownames(probes) <- rownames(exprsVals)[geneSymbols %in% g & probeSets %in% matchingProbes.de]
        ## Filter by prefer argument
        filter <- switch(prefer,
                         none = 1:length(matchingProbes.de), # no filter
                         up = which(matchingProbes.de %in% deProbes[["up"]]),
                         down = which(matchingProbes.de %in% deProbes[["up"]]))
        if(length(filter) > 0)
          probes <- probes[filter, , drop=F]
      }
      ## print(matchingProbes)
      ## print(matchingProbes.de)
    }
    ## print(probes)
    ## print(g)
    if (! singleProbeset ) {
      maxOfProbes[g,] = apply(probes,2,oper)
    } else { # only choose a single probeset for each duplicate
        probeAverages = rowSums(probes)/ncol(probes)
        if(debug) { print(head(probeAverages)) }
        ## Could be more than 1 match but only take the first one
        whichMax = which(probeAverages == oper(probeAverages))[1] 
        maxOfProbes[g,] = probes[whichMax,]
        maxProbeNames[g] <- rownames(probes)[whichMax]
      }
    ## Pause()
  }

  geneSymbols_Multiple = which( geneSymbols %in% geneSymbols_Multiple )
  ## if any gene symbols have multiples, remove these because they will be in maxOfProbes
  if(length(geneSymbols_Multiple) > 0) {
    geneSymbols <- geneSymbols[-geneSymbols_Multiple]
    exprsVals <- as.matrix(exprsVals[-geneSymbols_Multiple, ])
  }

  probeSets = c(rownames(exprsVals), maxProbeNames)
  names(probeSets) <- c(geneSymbols, names(maxProbeNames))
  rownames(exprsVals) = geneSymbols

  ## Reorder according to rownames (gene symbols) to be alphabetical
  exprMat <- rbind(exprsVals, maxOfProbes)
  ## show(head(rownames(exprMat)))
  ## show(head(exprMat))  
  reord <- order(rownames(exprMat))
  probeSets <- probeSets[reord]
  exprMat <- exprMat[reord,]

  if(returnProbes) # Return probes and the expression matrix
    return(list(exprsVals=exprMat, probes=probeSets))
  else # return only the expression matrix
    return(exprMat)
}

