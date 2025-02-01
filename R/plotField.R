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
	if(class(boundary) == "list"){
		if(length(boundary) > 1){
			otherBoundaries <- boundary[2:length(boundary)]
			plotOtherBoundLines <- TRUE
		}
		allPts <- do.call(rbind, lapply(boundary, slot, "points"))
		boundary <- boundary[[1]]
	} else {
		if(class(boundary) != "fieldBoundary" & is.matrix(boundary)){
			boundary <- matrixToDist(boundary)
		} else if(class(boundary) != "fieldBoundary"){
			stop("must provide an object of class 'fieldBoundary', a list of elements all with class 'fieldBoundary', or a 2 column matrix with x and y coords")
		}
		allPts <- boundary@points 
		plotOtherBoundLines <- FALSE
	}
	ftPoints <- boundary@points
	plot(allPts, asp = 1, type = 'n', bty = "l", xlab = units, ylab = units, ...)
	lines(rbind(ftPoints, ftPoints[1,]))
	if(plotOtherBoundLines){
		for(i in 1:length(otherBoundaries)){
			lines(rbind(otherBoundaries[[i]]@points, otherBoundaries[[i]]@points[1,]))
		}
	}
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
