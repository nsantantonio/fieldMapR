#' drawPlots function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param angle [value]
#' @param labAngle [value]. Default is 0
#' @param plotNos [value]. Default is TRUE
#' @param add [value]. Default is TRUE
#' @param ptrim [value]. Default is 1
#' @param rtrim [value]. Default is 2
#' @param color [value]. Default is NA
#' @param showBorderNum [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
drawPlots <- function(plots, angle, labAngle = 0, plotNos = TRUE, add = TRUE, ptrim = 1, rtrim = 2, color = NA, showBorderNum = FALSE, ...){
	
	trimPlots <- function(x) {
		x <- x + trim
		rbind(x, x[1,])
	}
	rdiff <- rtrim / 2
	pdiff <- ptrim / 2
	trim <- rotate(matrix(c(pdiff, -pdiff, -pdiff, pdiff, rdiff, rdiff, -rdiff, -rdiff), ncol = 2), angle)

	plotLines <- lapply(plots@corners, trimPlots)
	if(!add) plot(do.call(rbind, plotLines), asp = 1, type = 'n')

	if(length(color) == 1){
		color <- rep(color, length(plotLines))
	} else if(length(color) == 2){
		bcol <- color[2]
		color <- rep(color[1], length(plotLines))
		color[grep("^[A-z]", names(plotLines))] <- bcol
	} else {
		if(length(color) != length(plotLines)) stop("please provide a color vector of length 1, 2 (for borders), or number of plots!")
	}

	for(i in 1:length(plotLines)) {polygon(plotLines[[i]], col = color[i])}

	printPlotNames <- names(plots@centers)
	if(!showBorderNum){
		bp <- grep("^[A-z]", printPlotNames)
		printPlotNames[bp] <- gsub("[0-9]", "", printPlotNames[bp])
	}
	if(plotNos) text(do.call(rbind, plots@centers), labels = printPlotNames, pch = 16, srt = labAngle, ...)
}
