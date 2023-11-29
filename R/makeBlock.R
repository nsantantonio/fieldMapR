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
#' @examples # none
#' @export
makeBlock <- function(l, blockName, pile = TRUE, serpentinePlot = FALSE, startSerp = FALSE) {
	# l = trialPlots[c("HulledAdvance", "EasternMalt", "HulledPrelim", "WinterMalt", "AckermannMalt")]; blockName = "ScottsBackIII"; pile = TRUE; serpentinePlot = FALSE; startSerp = FALSE
	# l = trialPlots[c("DeWitt")]; blockName = "ScottsBackIV"; pile = TRUE; serpentinePlot = FALSE; startSerp = FALSE
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
	fillL <- lapply(l, slot, "fill")
	repL <- lapply(l, slot, "Rep")
	entL <- lapply(l, slot, "Entry")
	lineL <- lapply(l, slot, "Line")
	pedL <- lapply(l, slot, "Pedigree")
	if(length(unique(lapply(matL, dim))) > 1) stop("all fieldPlots must have the same sized matrix!")
	
	plotNos <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	fill <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	trials <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	reps <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	entries <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	lineNames <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	pedigrees <- matrix(NA, nrow(matL[[1]]), ncol(matL[[1]]))
	for(i in 1:length(matL)){
		index <- which(!is.na(matL[[i]]), arr.ind = TRUE) # dont think I actually need the arr.ind == TRUE here and it simplifies subsetting the index not to have fill plots
		fill[index] <- fillL[[i]][index] 
		trials[index] <- slot(l[[i]], "trialName")
		reps[index] <- repL[[i]][index] 
		plotNos[index] <- matL[[i]][index] 
		entries[index] <- entL[[i]][index] 
		lineNames[index] <- lineL[[i]][index] 
		pedigrees[index] <- pedL[[i]][index] 
		# index <- which(!is.na(matL[[i]]))
		# whichFill <- which(slot(l[[i]], "fill"), arr.ind = TRUE)
		whichFill <- which(fill, arr.ind = TRUE)
		# expIndex <- index[!index %in% whichFill]
		# if(length(slot(l[[i]], "Entry"))) entries[expIndex] <- slot(l[[i]], "Entry")
		# if(length(slot(l[[i]], "Line"))) lineNames[expIndex] <- slot(l[[i]], "Line")
		entries[whichFill] <- 0L
		reps[whichFill] <- 0L
		lineNames[whichFill] <- "fill"
	}

	# trials[is.na(trials)] <- "fill"
	# plotNos[is.na(plotNos)] <- fill
	plotNames <- plotNos
	plotNames[] <- paste0(trials, "_", plotNos)

	trialsR <- trials
	plotNosR <- plotNos
	repR <- reps
	# fillR <- fill
	lineNamesR <- lineNames
	entriesR <- entries
	plotNamesR <- plotNames
	ranges <- flipInt(row(trials))

	if(serpentinePlot){ 
		s <- if(startSerp) 1 else 2
		swap <- seq(s, ncol(plotNos), by = 2)
		trials[, swap] <- trials[nrow(trials):1, swap]
		plotNos[, swap]	<- plotNos[nrow(plotNos):1, swap]
		# fill[, swap]	<- fill[nrow(plotNos):1, swap]
		ranges[, swap]	<- ranges[nrow(ranges):1, swap]
		lineNames[, swap]	<- lineNames[nrow(lineNames):1, swap]
		reps[, swap]	<- reps[nrow(reps):1, swap]
		entries[, swap]	<- entries[nrow(entries):1, swap]
		plotNames[, swap]	<- plotNames[nrow(plotNames):1, swap]
	}

	passes <- col(trials)

	longframe <- data.frame(Trial = c(trials), Line = c(lineNames), Entry = c(entries), Rep = c(reps), plotName = c(plotNames), plotNo = c(plotNos), range = c(ranges), pass = c(passes), plantOrder = 1:prod(dim(plotNos)))

	fieldBlock(Trial = trialsR, Line = lineNamesR, Entry = entriesR, Rep = repR, plotName = plotNamesR, plotNo = plotNosR, fill = fill, long = longframe, borderPasses = bpasses[[1]], needStake = stakes, blockName = blockName)
}
