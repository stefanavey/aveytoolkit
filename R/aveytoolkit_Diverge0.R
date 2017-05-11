##' Diverge0
##'
##' Given data and colors, find colors and breaks where the center of the pallete is at 0
##'
##' @param data any numeric data type for which a single range can be calculated
##' @param ramp the name of an RColorBrewer palette (as character), a character
##'             vector of colour names to interpolate, or a colorRampPalette.
##' @param reverse logical specifying whether colors should be reversed (e.g. RdBu scale becomes Blue to Red). Default is FALSE.
##' @param maxColors the maximum number of colors you wish to interpolate. The number returned will be at most this number.
##' @return a named list with two elements: breaks and colors
##' @details Inspired by John Baumgartner's function to diverge a color scale for an image (see reference). I use this function often when making a heatmap and I have a diverging color pallete where I want the middle color (usually white) to map to a zero value.  When the distribution is skewed from 0 this doesn't happen by default using \code{pheatmap} (but it does happen automatically for some other heatmap functions like \code{heatmap.2}). This function can be used to get the colors and breaks to pass to the \code{pheatmap} function.
##' @import RColorBrewer
##' @author John Baumgartner, Stefan Avey
##' @references \url{https://gist.github.com/johnbaums/306e4b7e69c87b1826db}
##' @examples
##' \dontrun{
##' library(pheatmap)
##' hm <- matrix(rnorm(n = 100, mean = 1, sd = 1), nrow = 10, ncol = 10)
##' div <- Diverge0(data = hm, ramp = "RdBu", reverse = TRUE)
##' ## In this heatmap, 0 values appear blue which is misleading
##' pheatmap(mat = hm, rev(brewer.pal(11, "RdBu")))
##' ## In this heatmap, the diverging scale is centered around 0
##' pheatmap(mat = hm, color = div$colors, breaks = div$breaks)
##' 
##' }
##' 
##' @export
Diverge0 <- function(data, ramp, reverse = FALSE, maxColors = 256) {
  if (length(ramp) == 1 && is.character(ramp) && ramp %in% 
      row.names(brewer.pal.info)) {
    if (as.character(brewer.pal.info[ramp, "category"]) != "div") {
      stop("ramp should be a diverging color pallete, not qualitative or sequential.")
    }
    ramp <- suppressWarnings(colorRampPalette(brewer.pal(11, ramp)))
  } else if (length(ramp) > 1 && is.character(ramp) &&
             all(ramp %in% grDevices::colors())) {
      ramp <- grDevices::colorRampPalette(ramp)
    } else if (!is.function(ramp)) 
        stop('ramp should be either the name of a diverging RColorBrewer palette, ', 
             'a vector of colours to be interpolated, or a colorRampPalette.')

  rng <- range(data)
  s <- seq(-max(abs(rng)), max(abs(rng)), len=maxColors+1)
  i <- findInterval(rng[which.min(abs(rng))], s)
  zlim <- switch(which.min(abs(rng)), `1`=i:(maxColors+1), `2`=1:(i+1))
  if (reverse) {
    cols <- rev(ramp(maxColors))[zlim[-length(zlim)]]
  } else {
    cols <- ramp(maxColors)[zlim[-length(zlim)]]
    }
  breaks <- seq(rng[1], rng[2], len = length(cols) + 1)
  return(list(colors = cols, breaks = breaks))
}
