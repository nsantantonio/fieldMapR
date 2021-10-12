#' An S4 class to represent field plots
#' @name fieldPlots
#' @slot centers  list of centers
#' @slot corners  list of corners
#' @slot matrix  matrix of matrix
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot trialName  character of trialName
#' @export fieldPlots
fieldPlots <- setClass("fieldPlots", slots = c(centers = "list", corners = "list", matrix = "matrix", needStake = "list", borderPasses = "numeric", trialName = "character", Entry = "integer", Line = "character", Pedigree = "character"))

#' @export
cbind.fieldPlots <- function(...){
	l <- list(...)

	unL <- list()
	for(i in slotNames(l[[1]])){
		unL[[i]] <- lapply(l, slot, i)
	}
	if(length(unique(sapply(unL[["matrix"]], nrow))) > 1) stop("all fieldPlots must have the same number of ranges (matrix rows)")
	
	mat <- do.call(cbind, unL[["matrix"]])

	centers <- unlist(unL[["centers"]], recursive = FALSE)
	corners <- unlist(unL[["corners"]], recursive = FALSE)
	
	needStake <- unL[["needStake"]][[1]]
	borderPasses <- unL[["borderPasses"]][[1]]
	cc <- 0
	for(i in 2:length(l)){
		cc <- cc + ncol(unL[["matrix"]][[i-1]])
		stakei <- lapply(unL[["needStake"]][[i]], function(x) {x[2] <- x[2] + cc; x})
		needStake <- c(needStake, stakei)
		if(!is.na(unL[["borderPasses"]][[i]][1])) borderPasses <- c(borderPasses, unL[["borderPasses"]][[i]] + cc)
	}
	fieldPlots(centers = centers, corners = corners, matrix = mat, needStake = needStake, borderPasses = borderPasses, trialName = unL[["trialName"]][[1]])
}

#' @export
rbind.fieldPlots <- function(...){

	l <- list(...)

	unL <- list()
	for(i in slotNames(l[[1]])){
		unL[[i]] <- lapply(l, slot, i)
	}
	if(length(unique(sapply(unL[["matrix"]], ncol))) > 1) stop("all fieldPlots must have the same number of passes (matrix columns)")
	
	mat <- do.call(rbind, unL[["matrix"]])

	centers <- unlist(unL[["centers"]], recursive = FALSE)
	corners <- unlist(unL[["corners"]], recursive = FALSE)
	
	needStake <- unL[["needStake"]][[1]]
	rc <- 0
	for(i in 2:length(l)){
		rc <- rc + nrow(unL[["matrix"]][[i-1]])
		stakei <- lapply(unL[["needStake"]][[i]], function(x) {x[1] <- x[1] + rc; x})
		needStake <- c(needStake, stakei)
	}
	
	borderPasses <- unique(unL[["borderPasses"]])
	borderPasses <- borderPasses[!is.na(borderPasses)]
	if(length(borderPasses) > 1) print("multiple border passes, unexpected results may follow")
	borderPasses <- unlist(borderPasses)

	fieldPlots(centers = centers, corners = corners, matrix = mat, needStake = needStake, borderPasses = borderPasses, trialName = unL[["trialName"]][[1]])
}