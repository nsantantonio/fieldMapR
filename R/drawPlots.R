#' drawPlots function
#'
#' function to draw plots on an R graphics device. 
#'
#' @param plots object of class 'fieldPlots'
#' @param angle numeric. Angle of which to draw plots 
#' @param labAngle numeric. Angle at which to draw plot labels
#' @param plotNos logical. Should plot numbers be displayed? Default is TRUE
#' @param add logical. should plots be drawn on an existing R device? Default is TRUE
#' @param ptrim numeric. Distance to remove between passes when drawing plots. Example, passes may be 5 feet wide, but plots are only 4 feet wide, then ptrim should be 5-4=1. Default is 1
#' @param rtrim numeric. Distance to remove between passes when drawing plots. Example, passes may be 16 feet apart, but plots are then trimmed to 14 feet long, then rtrim should be 16-14=2.Default is 2
#' @param color character. color to be used for plots
#' @param showBorderNum logical. Should border numbers be shown?
#' @param ... arguments passed to text()
#' @return nothing
#' @details drawPlots draws a graphical representation of field plots to an R device, such as a pdf. 
#' @examples # none
#' @importFrom graphics arrows lines plot points polygon segments text
#' @export
drawPlots <- function(plots, angle = 0, labAngle = angle+90, plotNos = TRUE, add = TRUE, ptrim = 1, rtrim = 2, color = NA, showBorderNum = FALSE, ...){
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
