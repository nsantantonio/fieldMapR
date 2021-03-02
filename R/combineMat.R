#' combineMat function
#'
#' function to (do something)
#'
#' @param  [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
combineMat <- function(...) {
	l <- list(...)
	if(!all(sapply(l, class) == "fieldPlots")) stop("only objects of class 'fieldPlots' are allowed!")
	matL <- lapply(l, slot, "matrix")
	if(length(unique(lapply(matL, dim))) > 1) stop("all fieldPlots must have the same sized matrix!")
	
	plotNos <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	# if (makeTrialName) trials <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	for(i in 1:length(matL)){
		index <- which(!is.na(matL[[i]]), arr.ind = TRUE)
		plotNos[index] <- matL[[i]][index] 
		# trials[index] <- names(matL)[i]
		# if (makeTrialName) trials[index] <- slot(l[[i]], "trialName")
	}
	# rmatL <- list(matrix = plotNos)
	# if(makeTrialName) rmatL <- c(rmatL, list(trial = trials))
	# return(rmatL)
	plotNos
}
