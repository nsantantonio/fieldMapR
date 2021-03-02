#' printStakes function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param addPass [value]. Default is 0
#' @param every [value]. Default is 5
#' @param tooClose [value]. Default is 2
#' @param blockName [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
printStakes <- function(plots, addPass = 0, every = 5, tooClose = 2, blockName = NULL){
 	
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

 	ranges <- flipInt(row(m))
 	passes <- col(m) + addPass

 	getPasses <- function(x, rmPass = plots@borderPasses){
	 	xtras <- x[do.call(rbind, plots@needStake)]

	 	if(!is.na(rmPass[1])) {
	 		x <- x[, -rmPass]
	 	}

		nPasses <- ncol(x)
 		keepPass <- seq(0, nPasses, by = every)
 		keepPass[1] <- 1
	 	if(nPasses - tail(keepPass, 1) <= tooClose) {
	 		keepPass <- keepPass[-length(keepPass)]
	 	}
	 	keepPass <- c(keepPass, nPasses)

	 	c(x[,keepPass], xtras)
 	}
 	cm <- getPasses(m)
 	cpasses <- getPasses(passes)
 	cranges <- getPasses(ranges)
 	ctrial <- getPasses(trial)
 	longdf <- data.frame(block = blockName, trial = ctrial, plot = cm, pass = cpasses, range = cranges)
 	isUniq <- !duplicated(longdf)

	fieldStakes(plotNo = cm[isUniq], pass = cpasses[isUniq], range = cranges[isUniq], trial = ctrial[isUniq], block = blockName, long = longdf[isUniq,])

}
