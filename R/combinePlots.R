#' combinePlots function
#'
#' function to (do something)
#'
#' @param ... objects of class 'fieldPlots' to be combined. 
#' @param trialName [value]. Default is NULL
#' @param pile [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
combinePlots <- function(..., trialName = NULL, pile = NULL){
	l <- list(...)
	# x1 = l[[1]]; x2 = l[[2]]
	if(!all(sapply(l, class) == "fieldPlots")) stop("all supplied arguments must be of class 'fieldPlots'!")
	if(is.null(pile)){
		if(length(unique(lapply(list(...), function(x) dim(x@matrix)))) == 1) {
			canCombMat <- TRUE
		} else {
			canCombMat <- FALSE
		}
	} else {
		canCombMat <- pile
	}

	combdplots <- Reduce(function(x1, x2) combine(x1, x2, pile = canCombMat), list(...))
	if(!is.null(trialName)) combdplots@trialName <- trialName
	combdplots
}
