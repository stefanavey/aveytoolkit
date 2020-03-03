#' ggplot dual axis
#'
#' \code{aveytoolkit_ggDualAxis} Takes two ggplot objects and combines them onto a single plot with dual x or y axes.
#'
#' @param plot1 a ggplot2 object
#' @param plot2 a ggplot2 object
#' @param which.axis character vector of length 1 specifying "x" or "y" axis. Default if "y".
#'
#' @return NULL 
#' @author Jon Lefcheck
#' @references \url{https://gist.github.com/jslefche/e4c0e9f57f0af49fca87}
#' @import ggplot2
#' @export
#' @examples
#' ## Example for x axis
#' # Create fake data.frame
#' data.add.x = data.frame(
#'   y1 = runif(100, 0, 100),
#'   x1 = runif(100, 0, 100)
#' )
#' 
#' # Add second x-axis that scales with first
#' data.add.x$x2 = (data.add.x$x1 + 50)^0.75
#' 
#' # Create plots
#' plot1.x = qplot(y = y1, x = x1, data = data.add.x)
#' plot2.x = qplot(y = y1, x = x2, data = data.add.x, col = 2)
#' 
#' # Run function
#' ggDualAxis(plot1.x, plot2.x, "x")
#'
#' ## Example for y axis
#' # Add second y-axis that scales with first
#' data.add.x$y2 = (data.add.x$y^0.5) / 500
#' 
#' # Create plots
#' plot1.y = qplot(y = y1, x = x1, data = data.add.x)
#' plot2.y = qplot(y = y2, x = x1, data = data.add.x)
#' 
#' # Run function
#' ggDualAxis(plot1.y, plot2.y, "y")
ggDualAxis <- function(plot1, plot2, which.axis = c("y", "x")) {
  which.axis = match.arg(which.axis)
  # Update plot with transparent panel
  plot2 = plot2 + theme(panel.background = element_rect(fill = NA))
  grid::grid.newpage()
  # Increase right margin if which.axis == "y"
  if(which.axis == "y")
    plot1 = plot1 +
      theme(plot.margin = unit(c(0.7, 1.5, 0.4, 0.4), "cm"))
  # Extract gtable
  g1 = ggplot_gtable(ggplot_build(plot1))
  g2 = ggplot_gtable(ggplot_build(plot2))
  # Overlap the panel of the second plot on that of the first
  pp = c(subset(g1$layout, name == "panel", se = t:r))
  g = gtable::gtable_add_grob(g1, g2$grobs[[which(g2$layout$name=="panel")]], pp$t, pp$l, pp$b, pp$l)
  # Steal axis from second plot and modify
  axis.lab = ifelse(which.axis == "x", "axis-b", "axis-l")
  ia = which(g2$layout$name == axis.lab)
  ga = g2$grobs[[ia]]
  ax = ga$children[[2]]
  # Switch position of ticks and labels
  if(which.axis == "x")
    ax$heights = rev(ax$heights)
  else
    ax$widths = rev(ax$widths)
  ax$grobs = rev(ax$grobs)
  if(which.axis == "x") 
    
    ax$grobs[[2]]$y = ax$grobs[[2]]$y - unit(1, "npc") + unit(0.15, "cm") else
      
      ax$grobs[[1]]$x = ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
  
  # Modify existing row to be tall enough for axis
  if(which.axis == "x")
    g$heights[[2]] = g$heights[g2$layout[ia,]$t]
  
  # Add new row or column for axis label
  if(which.axis == "x") {
    g = gtable::gtable_add_grob(g, ax, 2, 4, 2, 4) 
    g = gtable::gtable_add_rows(g, g2$heights[1], 1)
    g = gtable::gtable_add_grob(g, g2$grob[[6]], 2, 4, 2, 4)
  } else {
    g = gtable::gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
    g = gtable::gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b) 
    g = gtable::gtable_add_grob(g, g2$grob[[7]], pp$t, length(g$widths), pp$b - 1)
  }
  # Draw it
  grid::grid.draw(g)
}
