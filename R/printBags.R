#' printBags function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param addPass numeric. number of passes to add. Default is 0
#' @param blockName chracter. name of block. Default is NULL
#' @param countBorder logical. Should the border passes be counted? Default is TRUE
#' @param printBorder logical. Should bags for the border passes be printed? Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
printBags <- function(plots, addPass = 0, countBorder = TRUE, blockName = NULL, printBorder = FALSE){
	# plots = blocks[[3]]; addPass = 0; countBorder = TRUE; blockName = NULL; printBorder = FALSE
	flipInt <- function(x) {(x - max(x) - 1) * -1}
 	
	if(class(plots) == "fieldPlots"){
		m <- plots@matrix[which(plots), drop = FALSE]
		Trial <- matrix(plots@trialName, nrow(m), ncol(m))
		# Entry <- plots@Entry # Note, this wont work when you have border plots!
		# Line <- plots@Line # Note, this wont work when you have border plots!
		if(is.null(blockName)) blockName <- "" 
	} else if(class(plots) == "fieldBlock"){
		m <- plots@plotNo
		Trial <- plots@Trial
		Entry <- plots@Entry
		Line <- plots@Line
		if(is.null(blockName)) blockName <- plots@blockName 
	} else {
		stop("must provide an object of class 'fieldPlots' or 'fieldBlock")
	}

# need to not print border passes even when they are counted. i.e. need counter. 
	if(!countBorder){
		if(!is.na(plots@borderPasses[1])) {
	 		m <- m[, -plots@borderPasses, drop = FALSE]
	 		Trial <- Trial[, -plots@borderPasses, drop = FALSE]
	 		Line <- Line[, -plots@borderPasses, drop = FALSE]
	 		Entry <- Entry[, -plots@borderPasses, drop = FALSE]
	 	}
	}	

 	ranges <- flipInt(row(m))
 	passes <- col(m) + addPass

 	getPasses <- function(x, rmPass = NA){
	 	if(!is.na(rmPass[1])) {
	 		x <- x[, -rmPass, drop = FALSE]
	 	}

	 	nPasses <- ncol(x)
	 	nRanges <- nrow(x)

		lastPass <- ceiling(nPasses / 2)
	 	forward <- 1:ceiling(nPasses / 2)
	 	if(ncol(x) > 1){
	 		reverse <- {lastPass + 1}:nPasses
	 		ret <- c(x[, forward], x[nRanges:1, reverse])
	 	} else {
	 		ret <- c(x)
	 	}
	 	return(ret)
 	}

 	if(countBorder & !printBorder) rmp <- plots@borderPasses else rmp <- NA
 	cm <- getPasses(m, rmp)
 	cpasses <- getPasses(passes, rmp)
 	cranges <- getPasses(ranges, rmp)
 	cTrial <- getPasses(Trial, rmp)
 	cPlotName <- paste0(cTrial, "_", cm)
 	cLine <- getPasses(Line, rmp)
 	cEntry <- getPasses(Entry, rmp)
 	longdf <- data.frame(Trial = cTrial, Line = cLine, Entry = cEntry, plotName = cPlotName, block = blockName, plot = cm, pass = cpasses, range = cranges)

	harvestBags(Trial = cTrial, Line = cLine, Entry = cEntry, plotName = cPlotName, plotNo = cm, pass = cpasses, range = cranges, block = blockName, long = longdf)

}
