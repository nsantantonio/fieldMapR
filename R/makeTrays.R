#' makeTrays function
#'
#' function to make headrow trays, particularly useful for making scab nursery trays. 
#'
#' @param plots either a list of trial designs, a single trial design or 
#' @param pile [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
makeTrays <- function(plots, traySize = 40, lastTray = 0, fill = c(2,2)) {
	if(!is.data.frame(plots) & is.list(plots)) {
		if(!all(sapply(plots, class)  == "trialDesign")){
			stop("If 'plots' is a list, it must contain all elements of class 'trialDesign'. Please provide either a single trialDesign, a list of trialDesigns or a single data.frame")
		} else {
			plots <- do.call(rbind, lapply(plots, tdToDf))
		}
	} else if(class(plots) == 'trialDesign'){
		plots <- tdToDf(plots)
	}
	nPlots <- nrow(plots)
	if(sum(fill) > 0){
		subTraySize <- traySize - sum(fill)
	} else {
		subTraySize <- traySize
	}
	fillEntry <- plots[1,]
	fillEntry[,] <- "FILL" 
	rownames(fillEntry) <- NULL

	nTray <- ceiling(nPlots / subTraySize)
	eachTray <- rep(1:nTray, each = subTraySize)[1:nrow(plots)]

	dfsplt <- split(plots, eachTray)

	trayL <- list()
	for(i in 1:nTray){
		trayi <- rbind(fillEntry[rep(1, fill[1]),], dfsplt[[i]], fillEntry[rep(1, fill[2]),])
		if(nrow(trayi) < traySize) trayi <- trayi <- rbind(trayi, fillEntry[rep(1, traySize - nrow(trayi)),])
		trayL[[i]] <- data.frame(tray = i, traySlot = 1:traySize, magazine = rep(1:10, each = 8 / 80 * traySize), slot = rep(1:{8 / 80 * traySize}, times = 10), range = rep(1:20, each = 4 / 80 * traySize), trayi)
	}

	trays <- do.call(rbind, trayL)
	rownames(trays) <- NULL
	return(trays)
}