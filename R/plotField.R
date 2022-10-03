#' plotField function
#'
#' function to (do something)
#'
#' @param boundary [value]
#' @param offset [value]. Default is 5
#' @param units [value]. Default is "ft"
#' @param hshift [value]. Default is 3
#' @param vshift [value]. Default is 1
#' @param showBoundry [value]. Default is TRUE
#' @param showPtNo [value]. Default is FALSE
#' @param sideLength [value]. Default is TRUE
#' @param ... arguments passed to text()
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
plotField <- function(boundary, offset = 5, units = "ft", hshift = 3, vshift = 1, showBoundry = TRUE, showPtNo = FALSE, sideLength = TRUE, ...){
	if(class(boundary) != "fieldBoundary" & is.matrix(boundary)){
		boundary <- matrixToDist(boundary)
	} 
	ftPoints <- boundary@points
	plot(ftPoints, asp = 1, type = 'n', bty = "l", xlab = units, ylab = units, ...)
	lines(rbind(ftPoints, ftPoints[1,]))
	if(showBoundry) points(ftPoints, pch = 4)
	if(showPtNo) text(ftPoints[-nrow(ftPoints),], labels = 1:{nrow(ftPoints)-1})

	if(sideLength){
		for(i in 2:length(boundary@distance)){		
			isVert <- boundary@angle[i] > 45 & boundary@angle[i] < 135 | boundary@angle[i] > 225 & boundary@angle[i] < 315
			offseti <- if(isVert) offset * hshift else offset * vshift
			pti <- shiftPt(colMeans(boundary@points[c(i-1, i), ]), a = boundary@angle[i], y = -offseti)
			text(pti[1], pti[2], labels = paste0(round(boundary@distance[i], 1), " ", units), ...)
		}
	}
}
