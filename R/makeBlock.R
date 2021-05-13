#' makeBlock function
#'
#' function to (do something)
#'
#' @param l [value]
#' @param blockName [value]
#' @param pile [value]. Default is TRUE
#' @param serpentinePlot [value]. Default is FALSE
#' @param startSerp [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export

makeBlock <- function(l, blockName, pile = TRUE, serpentinePlot = FALSE, startSerp = FALSE) {
	flipInt <- function(x) {(x - max(x) - 1) * -1}
	if(!is.list(l)) l <- list(l)
	bpasses <- unique(lapply(l, slot, "borderPasses"))
	if(length(bpasses) > 1) {
		warning("'borderPasses' dont match! First borderPasses will be kept. Unexpected behavior may result downstream")
	}
	stakes <- unique(unlist(lapply(l, slot, "needStake"), recursive = FALSE))
	# l <- list(...)
	if(!all(sapply(l, class) == "fieldPlots")) stop("only objects of class 'fieldPlots' are allowed!")
	matL <- lapply(l, slot, "matrix")
	if(length(unique(lapply(matL, dim))) > 1) stop("all fieldPlots must have the same sized matrix!")
	
	plotNos <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	trials <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	for(i in 1:length(matL)){
		index <- which(!is.na(matL[[i]]), arr.ind = TRUE)
		plotNos[index] <- matL[[i]][index] 
		trials[index] <- slot(l[[i]], "trialName")
	}
	# trials[is.na(trials)] <- "fill"
	# plotNos[is.na(plotNos)] <- fill
	trialsR <- trials
	plotNosR <- plotNos
	ranges <- flipInt(row(trials))

	if(serpentinePlot){
		s <- if(startSerp) 1 else 2
		swap <- seq(s, ncol(plotNos), by = 2)
		trials[, swap] <- trials[nrow(trials):1, swap]
		plotNos[, swap]	<- plotNos[nrow(plotNos):1, swap]
		ranges[, swap]	<- ranges[nrow(ranges):1, swap]
	}

	passes <- col(trials)

	longframe <- data.frame(trial = c(trials), plotNo = c(plotNos), range = c(ranges), pass = c(passes), plantOrder = 1:prod(dim(plotNos)))

	fieldBlock(trial = trialsR, plotNo = plotNosR, long = longframe, borderPasses = bpasses[[1]], needStake = stakes, blockName = blockName)
}
