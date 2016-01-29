##' annotatePDFs
##' 
##' Add content on top of existing PDF files and can combine them into one file
##' using command line tools
##'
##' This function is a wrapper around some shell commands that allows you to annotate
##' multiple PDF files and combine them into 1. This wrapper requires ghostscript
##' and coherent PDF (cpdf) \url{http://www.coherentpdf.com/} on the host machine
##' 
##' @param host name of the host (e.g. <<user>>@@<<IP Address>>)
##' @param inFiles a character vector giving the current PDF names
##' @param outFiles a character vector of filenames to save PDFs to. Only the first
##'        value is used if \code{combineMultiple} is TRUE
##' @param cytoscapeWindowList a list of cytoscapeWindow objects for the windows to
##'        save
##' @param filenames a character vector of filenames to save PDFs to. Only the first
##'        value is used if \code{combineMultiple} is TRUE
##' @param titles a character vector of titles for each plot in the same order as
##'        cytoscapeWindowList (default is to use the title slot in the
##'        cytoscapeWindowList). Use NULL to omit a title. If only 1 title is given,
##'        it will be put on all plots.
##' @param footers a character vector of footers for each plot in the same order
##'        as cytoscapeWindowList (default is NULL for no footers). If only 1 footer
##'        is given, it will be put on all plots.
##' @param combineMultiple Should multiple windows be combined into a single PDF with
##'        one image per page? (Default TRUE)
##' @param removeMultiple Should the temporary files be removed (use only if combining PDFs)?
##' @return an (invisible) list of exit codes for each bash operation
##' @author Stefan Avey
##' @export
##' @examples
##'
##' ## Still keeps the original files because
##' ## removeMultiple is FALSE
##' annotatePDFs(inFiles=paste0("inFile", 1:2)
##'              outFiles=paste0("outFile", 1:2),
##'              titles=paste0("testTitle", 1:2),
##'              footers=paste0("testFooter", 1:2),
##'              combineMultiple=TRUE,
##'              removeMultiple=FALSE)
annotatePDFs <- function(host="localhost", inFiles, outFiles,
                         titles=NULL, footers=NULL,
                         combineMultiple=TRUE, removeMultiple=combineMultiple)

  {
    ## Error handling
    if(any(nchar(basename(inFiles)) > 255)) {
      stop("Some names in inFile are longer than 255 characters")
    }
    if(any(nchar(basename(outFiles)) > 255)) {
      stop("Some names in outFiles are longer than 255 characters")
    }
    if(combineMultiple) {
      if(length(outFiles) > 1) {
        warning("More than 1 file name given when combineMultiple is TRUE, only the ",
                "first name will be used.")
        outFiles <- outFiles[1]
      }
      if( (length(inFiles) < 2) && combineMultiple) {
        warning("Less than 2 inFiles specified, setting combineMultiple and removeMultiple to FALSE")
        combineMultiple <- removeMultiple <- FALSE
      }
    } else {
      if(length(inFiles) != length(outFiles)) {
        stop("inFiles and outFiles must have the same length when combineMultiple is FALSE")
      }
    }
  
    ## Create the bash command
    cdCmd <- paste0("cd ", dirname(inFiles), "; ")
    bashCmd <- list()
    if(!is.null(titles)) {
      bashCmd[["titles"]] <- paste0(cdCmd,
                                    "cpdf -top 45 -font-size 40 -add-text \"", titles, "\" ",
                                    inFiles, " -o ", inFiles, collapse="; ")
    }
    if(!is.null(footers)) {
      bashCmd[["footers"]] <- paste0(cdCmd,
                                     "cpdf -bottom 20 -font-size 14 -add-text \"",
                                     footers, "\" ", inFiles, " -o ", inFiles, collapse="; ")
    }
    if(combineMultiple) {
      bashCmd[["combineMultiple"]] <- paste0(cdCmd,
                                             "gs -q -sPAPERSIZE=letter -dNOPAUSE ",
                                             "-dBATCH -sDEVICE=pdfwrite -sOutputFile=",
                                             basename(outFiles), " ",
                                             paste(basename(inFiles), collapse=' '), "; ")
      if(removeMultiple) {
        bashCmd[["removeMultiple"]] <-  paste0(cdCmd,
                                               "rm ",
                                               paste(basename(inFiles), collapse=' '))
      }
    }
    ## Run the bash command
    if(length(bashCmd) > 0) {
      if(host == "localhost") {
        ret <- lapply(bashCmd, function(cmd) system(cmd))
      } else {
        ret <- lapply(bashCmd, function(cmd) system(paste0("ssh ", host, " '", cmd, "'")))
      }
    } else {
      ret <- NULL
    }
    return(invisible(ret))
  }
