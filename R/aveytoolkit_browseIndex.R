#' browseIndex
#'
#' Simple function to open HTML page of index in the default browser
#'
#' @param package a string with the name of package to use
#' @param lib.loc a string of the directory name of the R library, or ‘NULL’.  The default value of ‘NULL’ corresponds to all libraries currently known.
#' @author Stefan Avey
#' @details Only works for a single package. Could improve to list an HTML page with multiple packages that you could then choose from.  Not currently implemented.  Borrows the conecpts from utils::browseVignettes
#' @keywords aveytoolkit
#' @export
#' @examples
#' browseIndex("utils")
browseIndex <- function(package, lib.loc=NULL) {
  path <- file.path(find.package(package, lib.loc, quiet = TRUE), "html", "00Index.html")
  browseURL(paste0("file:", path))
}
