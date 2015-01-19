##' browseIndex
##'
##' Simple function to open HTML page of index in the default browser
##'
##' @param package a string with the name of package to use. If no name is supplied, the most recently loaded package is used.
##' @param lib.loc a string of the directory name of the R library, or ‘NULL’.  The default value of ‘NULL’ corresponds to all libraries currently known.
##' @return invisibly, the URL passed to browseURL (i.e. "file:" + <index_filename>)
##' @author Stefan Avey
##' @details Only works for a single package. Could improve to list an HTML page with multiple packages that you could then choose from.  Not currently implemented.  Borrows the conecpts from utils::browseVignettes
##' @keywords aveytoolkit
##' @export
##' @examples
##' browseIndex("utils")
browseIndex <- function(package=NULL, lib.loc=NULL) {
  if(is.null(package)) {
    packages <- grep("^package:", search(), value=TRUE)
    if(!length(packages)) {
      stop("No packages found using `search()`")
    } else {
      package <- gsub("^package:", "", packages[1])
    }
  }
  path <- file.path(find.package(package, lib.loc, quiet = TRUE), "html", "00Index.html")
  theURL <- paste0("file:", path)
  browseURL(theURL)
  return(invisible(theURL))
}
