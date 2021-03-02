#' stackPlots function
#'
#' function to (do something)
#'
#' @param plotList [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
stackPlots <- function(plotList){
	newList <- list()
	corners <- unlist(lapply(plotList, "[[", "corners"), recursive = FALSE)
	centers <- unlist(lapply(plotList, "[[", "centers"), recursive = FALSE)

	# looks like I wanted to remake combineMat() here, need to follow up on whether combineMat is good, I dont use it now
	for(i in names(plotList)){

	}

}
