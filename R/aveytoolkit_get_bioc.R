##' get_bioc
##'
##' Get and install Bioconductor packages by name 
##'
##' @param ... The unquoted names of Bioconductor packages to install
##' @return a character vector containing the names of packages passed into the
##'     function (for which installation was attempted but not necessarily
##'     completed)
##' @details This is a convenience function that prevents need to copy/paste
##'     from Bioconductor website to get the biocLite function
##' @importFrom rlang enquos quo_name
##' @importFrom purrr map
##' @author Robert Amezquita, Stefan Avey
##' @examples
##' \dontrun{
##' ## Install Mfuzz and qpcrNorm Bioconductor packages
##' get_bioc(Mfuzz, qpcrNorm)
##' }
##' @export
get_bioc <- function(...) {
    if (!exists("biocLite")) {
        source("https://bioconductor.org/biocLite.R")
    }
    args <- rlang::enquos(...)
    args_char <- purrr::map(args, rlang::quo_name)
    args_sqsh <- as.character(unlist(args_char))
    biocLite(args_sqsh)
    return(invisible(args_sqsh))
}
