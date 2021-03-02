#' plotField function
#'
#' function to (do something)
#'
#' @param boundry [value]
#' @param offset [value]. Default is 5
#' @param units [value]. Default is "ft"
#' @param hshift [value]. Default is 3
#' @param vshift [value]. Default is 1
#' @param showBoundry [value]. Default is TRUE
#' @param showPtNo [value]. Default is FALSE
#' @param sideLength [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
plotField <- function(boundry, offset = 5, units = "ft", hshift = 3, vshift = 1, showBoundry = TRUE, showPtNo = FALSE, sideLength = TRUE, ...){
	ftPoints <- boundry$points
	plot(ftPoints, asp = 1, type = 'n', bty = "l", xlab = units, ylab = units, ...)
	lines(rbind(ftPoints, ftPoints[1,]))
	if(showBoundry) points(ftPoints, pch = 4)
	if(showPtNo) text(ftPoints[-nrow(ftPoints),], labels = 1:{nrow(ftPoints)-1})

	if(sideLength){
		for(i in 2:length(boundry$distance)){		
			isVert <- boundry$angle[i] > 45 & boundry$angle[i] < 135 | boundry$angle[i] > 225 & boundry$angle[i] < 315
			offseti <- if(isVert) offset * hshift else offset * vshift
			pti <- shiftPt(colMeans(boundry$points[c(i-1, i), ]), a = boundry$angle[i], y = -offseti)
			text(pti[1], pti[2], labels = paste0(round(boundry$distance[i], 1), " ", units), ...)
		}
	}
}
