##' ahtsummary
##'
##' print out the head and tail as well as a data summary
##'
##' @param x variable to summarize at head and tail
##' @param hlength number of rows to include at head
##' @param tlength number of rows to include at tail
##' @param digits digits to show for numeric values
##' @return returns NULL invisibly
##' @importFrom knitr kable
##'
##' @author ESS R package, modified by Stefan Avey
##'
##' @details knitr::kable is used if knitr is installed and `markdown == TRUE`
##' 
##' @export
ahtsummary <- function(x, hlength = 5, tlength = 5, digits = 2, markdown = FALSE) {
    .convert <- function(x) {
        if (require("knitr") && markdown) {
            return(knitr::kable(x))
        } else {
            return(base::I(x))
            }
    }
    ## fixme: simplify and generalize
    snames <- c("mean", "sd", "min", "max", "nlev", "NAs")
    d <- " "
    num_sumr <- function(x){
        c(f(mean(x, na.rm = TRUE)),
          f(sd(x, na.rm = TRUE)),
          f(min(x, na.rm = TRUE)),
          f(max(x, na.rm = TRUE)),
          d,
          f(sum(is.na(x), na.rm = TRUE)))
    }
    f <- function(x) format(x, digits = digits)

    if (is.data.frame(x) | is.matrix(x)) {
        if (nrow(x) <= tlength + hlength){
            print(.convert(x))
        } else {
            if (is.matrix(x))
                x <- data.frame(unclass(x))
            ## conversion needed, to avoid problems with derived classes suchs
            ## as data.table
            h <- as.data.frame(head(x, hlength))
            t <- as.data.frame(tail(x, tlength))
            for (i in 1:ncol(x)) {
                h[[i]] <- f(h[[i]])
                t[[i]] <- f(t[[i]])
            }
            ## summaries
            sumr <- sapply(x, function(c){
                if (is.logical(c))
                    ## treat logical as numeric; it's harmless
                    c <- as.integer(c)
                if (is.numeric(c))
                    num_sumr(c)
                else if (is.factor(c)) c(d, d, d, d, nlevels(c), sum(is.na(c)))
                else rep.int(d, length(snames))
            })
            sumr <- as.data.frame(sumr)
            row.names(sumr) <- snames
            dots <- rep("...", ncol(x))
            empty <- rep.int(" ", ncol(x))
            lines <- rep.int(" ", ncol(x))
            df <- rbind(h, ... = dots, t, `_____` = lines, sumr, ` ` = empty)
            print(.convert(df))
        }
    } else {
        cat("head(", hlength, "):\n", sep = "")
        print(.convert(head(x, hlength)))
        if (length(x) > tlength + hlength){
            cat("\ntail(", tlength, "):\n", sep = "")
            print(.convert(tail(x, tlength)))
        }
        cat("_____\n")
        if (is.numeric(x) || is.logical(x))
            print(structure(num_sumr(x), names = snames), quote = FALSE)
        else if (is.factor(x)){
            cat("NAs: ", sum(is.na(x), na.rm = TRUE), "\n")
            cat("levels: \n")
            print(levels(x))
        }
    }
    invisible(NULL)
}
