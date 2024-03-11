#' makeTrialLabels function
#'
#' function to (do something)
#'
#' @param labPos matrix with 2 columns indicating x and y positions of labels, or a list of vectors of length 2, with x and y coordinates per label, or a list of fieldPlots objects.
#' @param shiftLab list of vector of vertical adjustments for trial labels, useful when fieldplots are provided to labPos, to adjust the label position vertically
#' @param labNames vector of label names. 
#' @param cols colors to be used for label names
#' @param a numeric. angle at which labels should be drawn. Default is 0.
#' @param labh numeric. label height. Default is 8
#' @param labw numeric. label width. Default is 60
#' @param rotateLabs numjeric. rotation for labels. Default is 0
#' @param labPlots logical. Should plots be labeled? Useful for map development when set to FALSE to not print labels. Default is TRUE
#' @param ... additional arguments passed to text()
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
makeTrialLabels <- function(labPos, shiftLab, labNames, cols, a = 0, labh = 8, labw = 60, rotateLabs = 0, labPlots = TRUE, ...){
	if (is.list(labPos)) { 
		if (all(unique(sapply(labPos, class)) %in% c("fieldPlots", "fieldBlocks"))) {
			labPos <- do.call(rbind, lapply(labPos, getCenter))
		} else if (all(sapply(labPos, length) == 2)) {
			labPos <- do.call(rbind, labPos)
		}
	}

	labh <- labh / 2
	labw <- labw / 2
	labBoxRef <- rotate(rbind(c(-labw, -labh), c(labw, -labh), c(labw, labh), c(-labw, labh), c(-labw, -labh)), a)

	labBox <- list()
	for(i in 1:nrow(labPos)) {
		labPos[i, ] <- anglePoint(shiftLab[[i]], a+90, ref = labPos[i, ])

		labBox[[i]] <- sweep(labBoxRef, 2, labPos[i, ], "+")

	}
	names(labBox) <- names(shiftLab)


	if(labPlots){
		for(i in 1:length(labBox)) polygon(labBox[[i]], col = cols[[i]])
		text(labPos, labels = labNames, srt = a + rotateLabs, ...)
	}

}
