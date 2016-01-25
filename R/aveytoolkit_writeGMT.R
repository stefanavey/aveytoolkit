#################################################################################
## aveytoolkit_writeGMT.R
## 
##
## Short Summary: Write out a GMT (Gene Matrix Transposed) file.
##
## Copyright 2016 Stefan Avey
##
## Author: Stefan Avey <stefan.avey@yale.edu>
## Date Created: 2016/01/25 16:21:33
## Version: $Id: aveytoolkit_writeGMT.R, v 0.0
## Project Directory: /home/stefan/R/myPackages/aveytoolkit/R/
## Keywords: file I/O; helper; gene sets
##
## Description:
##
## Take in a vector of set names, descriptions, and gene identifiers and
## write them to a GMT file format.
## http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#GMT:_Gene_Matrix_Transposed_file_format_.28.2A.gmt.29
##
## Header generated automatically by TEMPLATE.R.tpl
#################################################################################

##' @param filename the file to write to (should include '.gmt' extension
##' @param sets a list of character vectors containing the sets to write
##' @param setNames a character vector of set names corresponding to sets.
##'        Defaults to 1, 2, 3, ..., length(sets) if nonte specified.
##' @param setDescriptions a character vector of set descriptions corresponding
##'        to sets. Defaults to \code{NA} values if none specified.
##' @return 
writeGMT <- function(filename, sets, setNames = names(sets),
                     setDescriptions = rep(NA, length(sets))) {
  lines <- vector(mode = "character", length=length(sets))
  if(length(sets) != length(setNames)) {
    stop("`setNames` and `sets` must be the same length")
  }
  if(length(sets) != length(setDescriptions)) {
    stop("`setDescriptions` and `sets` must be the same length")
  }
  for(i in seq_along(sets)) {
    lines[i] <- paste(setNames[i], setDescriptions[i],
                       paste0(sets[[i]], collapse = '\t'), sep='\t')
  }
  write(lines, file = filename, sep = '\n')
}


