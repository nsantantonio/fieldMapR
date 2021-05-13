#' printBags function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param addPass [value]. Default is 0
#' @param blockName [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export

printBags <- function(plots, addPass = 0, countBorder = TRUE, blockName = NULL){
	flipInt <- function(x) {(x - max(x) - 1) * -1}
 	
	if(class(plots) == "fieldPlots"){
		m <- plots@matrix
		trial <- matrix(plots@trialName, nrow(m), ncol(m))
		if(is.null(blockName)) blockName <- "" 
	} else if(class(plots) == "fieldBlock"){
		m <- plots@plotNo
		trial <- plots@trial
		if(is.null(blockName)) blockName <- plots@blockName 
	} else {
		stop("must provide an object of class 'fieldPlots' or 'fieldBlock")
	}

	if(!countBorder){
		if(!is.na(plots@borderPasses[1])) {
	 		m <- m[, -plots@borderPasses]
	 		trial <- trial[, -plots@borderPasses]
	 	}
	}	

 	ranges <- flipInt(row(m))
 	passes <- col(m) + addPass

 	getPasses <- function(x, rmPass = NA){
	 	if(!is.na(rmPass[1])) {
	 		x <- x[, -rmPass]
	 	}

	 	nPasses <- ncol(x)
	 	nRanges <- nrow(x)

		lastPass <- ceiling(nPasses / 2)
	 	forward <- 1:ceiling(nPasses / 2)
	 	reverse <- {lastPass + 1}:nPasses

	 	c(x[, forward], x[nRanges:1, reverse])

 	}
 	cm <- getPasses(m)
 	cpasses <- getPasses(passes)
 	cranges <- getPasses(ranges)
 	ctrial <- getPasses(trial)
 	longdf <- data.frame(block = blockName, trial = ctrial, plot = cm, pass = cpasses, range = cranges)

	harvestBags(plotNo = cm, pass = cpasses, range = cranges, trial = ctrial, block = blockName, long = longdf)

}
