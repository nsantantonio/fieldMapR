#' drawLegend function
#'
#' function to (do something)
#'
#' @param plotList [value]
#' @param boundry [value]
#' @param shiftLeg [value]
#' @param cols [value]
#' @param scaleLeg [value]. Default is 0.33
#' @param rotateLabs [value]. Default is 0
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
drawLegend <- function(plotList, boundry, cols, shiftLeg = c(0,0), labNames = NULL, scaleLeg = 0.33, rotateLabs = 0, ...){
	if(is.null(labNames)) labNames <- sapply(plotList, function(x) x@trialName)
	ftPoints <- boundry$points
	ftPointsLeg <- sweep(rbind(ftPoints, ftPoints[1,]) * scaleLeg, 2, shiftLeg, "+")
	lines(ftPointsLeg)

	polys <- lapply(plotList, getOutline)
	polysLeg <- lapply(polys, function(x) sweep(x * scaleLeg, 2, shiftLeg, "+"))
	for(i in 1:length(plotList)){
		polygon(polysLeg[[i]], col = cols[[i]])
	}
	text(sweep(centers * scaleLeg, 2, shiftLeg, "+"), labels = labNames, srt = a + rotateLabs, ...)
}
