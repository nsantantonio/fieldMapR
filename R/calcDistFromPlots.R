#' calcDistFromPlots function
#'
#' function to (do something)
#'
#' @param plots1 [value]
#' @param plotNo1 [value]
#' @param corner1 [value]
#' @param plots2 [value]
#' @param plotNo2 [value]
#' @param corner2 [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
calcDistFromPlots <- function(plots1, plotNo1, corner1, plots2, plotNo2, corner2, ...){

	pt1 <- getCoord(plots1, plotNo1, corner1)
	pt2 <- getCoord(plots2, plotNo2, corner2) 	

	calcDist(pt1, pt2, ...)
}
