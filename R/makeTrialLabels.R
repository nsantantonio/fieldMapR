#' makeTrialLabels function
#'
#' function to (do something)
#'
#' @param labPos [value]
#' @param shiftLab [value]
#' @param labNames [value]
#' @param cols [value]
#' @param a [value]. Default is 0
#' @param labh [value]. Default is 8
#' @param labw [value]. Default is 60
#' @param rotateLabs [value]. Default is 0
#' @param labPlots [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
makeTrialLabels <- function(labPos, shiftLab, labNames, cols, a = 0, labh = 8, labw = 60, rotateLabs = 0, labPlots = TRUE, ...){
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
