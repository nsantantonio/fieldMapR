#' shiftPlot function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param plotNo [value]
#' @param ang1 [value]
#' @param ang2 [value]. Default is ang1
#' @param offsetx [value]. Default is 0
#' @param offsety [value]. Default is 0
#' @param corner [value]. Default is "bottomright"
#' @param plotref [value]. Default is FALSE
#' @param plotstart [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
shiftPlot <- function(plots, plotNo, ang1, ang2 = ang1, offsetx = 0, offsety = 0, corner = "bottomright", plotref = FALSE, plotstart = FALSE){
	ref <- getCoord(plots = plots, plotNo = plotNo, corner = corner)
	if(plotref) points(ref[[1]], ref[[2]], pch = 1, cex = 2, col = "red")
	start <- anglePoint(d = offsety, ang1 - 90, ref = ref)
	start <- anglePoint(d = offsetx, ang2, ref = start)
	if(plotstart) points(start[[1]], start[[2]], pch = 3, cex = 2, col = "red")
	if(plotstart & plotref) segments(ref[1], ref[2], start[1], start[2], col = "red")
	start
}
