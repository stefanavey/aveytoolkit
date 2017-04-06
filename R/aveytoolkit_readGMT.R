#' readGMT.R
#' 
#' Read in a GMT (Gene Matrix Transposed) file.
#' 
#' @author Stefan Avey.
#' @details Read in a vector of set names, descriptions, and gene identifiers and
#'          store them in a list. For more details on the GTM format, see
#'          \url{http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29}. This code is heavily adapted from `qusage::read.gmt()` and `GSA::GSA.read.gmt()` to take the best of both worlds.
#' @param filename the GTM file to read in (must end in .gmt)
#' @param trimMissing logical indicating whether to trim missing gene identifiers that are read as empty strings (default is TRUE)
#' @param quiet logical indicating whether to show how many records read or stay quiet. Default is to pass quiet = FALSE to `scan()`
#'
#' @return a list with the following elements
#'
#'\describe{
#'   \item{genesets}{a list with one element per gene set containing a character vector of genes}
#'   \item{names}{a list with one element per gene set containing the set names}
#'   \item{descriptions}{a list with one element per gene set containing the set descriptions}
#' }
#' @export
readGMT <- function(filename, trimMissing = TRUE, quiet = FALSE) {
  if (!grepl("\\.gmt$", filename)[1]) {
    stop("Pathway information must be a .gmt file")
  }
  ## Read in name and description from each line of file
  meta <- scan(filename, what = list(character(1), character(1)),
               sep = "\t", quote = NULL, fill = TRUE, flush = TRUE,
               multi.line = FALSE, quiet = quiet)
  ## Read in entire file to get the genesets
  genesets <- readLines(filename)
  genesets <- strsplit(genesets, '\t')
  ## Remove meta data on name and description
  genesets <- lapply(genesets, "[",-1:-2)
  if(trimMissing) {
    genesets <- lapply(genesets, function(x) { x[which(x != "")] })
  }
  ## Create a list to return
  out <- list(genesets = genesets, names = meta[[1]], descriptions = meta[[2]])
  return(out)
}
