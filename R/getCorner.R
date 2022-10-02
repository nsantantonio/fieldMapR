#' getCorner function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param whichCorner [value]. Default is "bottomright"
#' @param whichPlotCorner [value]. Default is whichCorner
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
getCorner <- function(plots, whichCorner = "bottomright", whichPlotCorner = whichCorner){
	pos <- c(bottomleft = 1, bottomright = 2, topright = 3, topleft = 4)
	m <- plots@matrix
	mNoNA <- m[rowSums(is.na(m)) < ncol(m),, drop = FALSE]

	x <- if(grepl("bottom", whichCorner)) nrow(mNoNA) else 1
	y <- if(grepl("right", whichCorner)) ncol(mNoNA) else 1

	if(any(is.na(mNoNA))){
		xr <- mNoNA[x,]
		xrNoNA <- xr[!is.na(xr)]
		plotNo <- if(grepl("left", whichCorner)) xrNoNA[1] else xrNoNA[length(xrNoNA)]
	} else {
		plotNo <- mNoNA[x, y]
	}
	
	return(plots@corners[[plotNo]][pos[whichPlotCorner],])
}
