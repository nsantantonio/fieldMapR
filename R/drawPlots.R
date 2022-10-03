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
#' @param ... arguments passed to text()
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @importFrom graphics arrows lines plot points polygon segments text
#' @export
drawPlots <- function(plots, angle, labAngle = 0, plotNos = TRUE, add = TRUE, ptrim = 1, rtrim = 2, color = NA, showBorderNum = FALSE, ...){
	# plots = breederSeed; rtrim = c(0, 15); angle = hrAng; labAngle = hrAng - 90; color = c(colsAlpha[trialColors["breederSeed"]], "gray"); cex = plotCex; plotNos = showBno; showBorderNum = TRUE; ptrim = 1; rtrim = 2; add = TRUE;
	trimPlots <- function(x) {
		x <- x + trim
		rbind(x, x[1,])
	}
	if(length(ptrim) == 1) pdiff <- rep(ptrim / 2, 2) else pdiff <- ptrim[1:2]
	if(length(rtrim) == 1) rdiff <- rep(rtrim / 2, 2) else rdiff <- rtrim[1:2]

	# rdiff <- rtrim / 2
	# pdiff <- ptrim / 2
	# trim <- rotate(matrix(c(pdiff, -pdiff, -pdiff, pdiff, rdiff, rdiff, -rdiff, -rdiff), ncol = 2), angle)
	trim <- rotate(matrix(c(pdiff[1], -pdiff[2], -pdiff[2], pdiff[1], rdiff[1], rdiff[1], -rdiff[2], -rdiff[2]), ncol = 2), angle)

	plotLines <- lapply(plots@corners, trimPlots)
	if(!add) plot(do.call(rbind, plotLines), asp = 1, type = 'n')

	if(length(color) == 1){
		color <- rep(color, length(plotLines))
	} else if(length(color) == 2){
		bcol <- color[2]
		color <- rep(color[1], length(plotLines))
		color[names(plotLines) %in% plots@matrix[plots@fill]] <- bcol
	} else {
		if(length(color) != length(plotLines)) stop("please provide a color vector of length 1, 2 (for borders), or number of plots!")
	}

	for(i in 1:length(plotLines)) {polygon(plotLines[[i]], col = color[i])}

	printPlotNames <- names(plots@centers)
	if(!showBorderNum){
		# bp <- grep("^[A-z]", printPlotNames)
		bp <- which(names(plotLines) %in% plots@matrix[plots@fill])
		printPlotNames[bp] <- gsub("[0-9]", "", printPlotNames[bp])
	}
	if(plotNos) text(do.call(rbind, plots@centers), labels = printPlotNames, pch = 16, srt = labAngle, ...)
}
