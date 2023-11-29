#' An S4 class to represent field plots
#' @name fieldPlots
#' @slot centers  list of centers
#' @slot corners  list of corners
#' @slot matrix  matrix of matrix
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot trialName  character of trialName
#' @importFrom methods new slot slot<- slotNames
#' @export fieldPlots
fieldPlots <- setClass("fieldPlots", slots = c(centers = "list", corners = "list", matrix = "matrix", fill = "matrix", needStake = "list", borderPasses = "numeric", trialName = "character", Rep = "matrix", Entry = "matrix", Line = "matrix", Pedigree = "matrix"))

#' @export
cbind.fieldPlots <- function(...){
	l <- list(...)

	unL <- list()
	for(i in slotNames(l[[1]])){
		unL[[i]] <- lapply(l, slot, i)
	}
	if(length(unique(sapply(unL[["matrix"]], nrow))) > 1) stop("all fieldPlots must have the same number of ranges (matrix rows)")
	
	mat <- do.call(cbind, unL[["matrix"]])
	fillmat <- do.call(cbind, unL[["fill"]])
	repmat <- do.call(cbind, unL[["Rep"]])
	linemat <- do.call(cbind, unL[["Line"]])
	entmat <- do.call(cbind, unL[["Entry"]])
	pedmat <- do.call(cbind, unL[["Pedigree"]])

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
	fieldPlots(centers = centers, corners = corners, matrix = mat, fill = fillmat, Rep = repmat, Line = linemat, Entry = entmat, Pedigree = pedmat, needStake = needStake, borderPasses = borderPasses, trialName = unL[["trialName"]][[1]])
}

#' @export
rbind.fieldPlots <- function(...){

	l <- list(...)
	# l <- list(fillFront, ObsILtrial, fillBack)
	unL <- list()
	for(i in slotNames(l[[1]])){
		unL[[i]] <- lapply(l, slot, i)
	}
	if(length(unique(sapply(unL[["matrix"]], ncol))) > 1) stop("all fieldPlots must have the same number of passes (matrix columns)")
	
	mat <- do.call(rbind, unL[["matrix"]])
	fillmat <- do.call(rbind, unL[["fill"]])
	repmat <- do.call(rbind, unL[["Rep"]])
	linemat <- do.call(rbind, unL[["Line"]])
	entmat <- do.call(rbind, unL[["Entry"]])
	pedmat <- do.call(rbind, unL[["Pedigree"]])

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
	if(length(borderPasses) == 1) {
		borderPasses <- borderPasses[[1]] 
	} else {
		borderPasses <- borderPasses[!is.na(borderPasses)]
		if(length(borderPasses) > 1) print("multiple border passes, unexpected results may follow")
		borderPasses <- unlist(borderPasses)
	}

	fieldPlots(centers = centers, corners = corners, matrix = mat, fill = fillmat, Rep = repmat, Line = linemat, Entry = entmat, Pedigree = pedmat, needStake = needStake, borderPasses = borderPasses, trialName = unL[["trialName"]][[1]])
}

#' @export
length.fieldPlots <- function(x){
	sum(!is.na(x@matrix))
}


#' @export
c.fieldPlots <- function(x){
	m <- x@matrix	
	mL <- rev(split(m, row(m)))
	if(length(mL) > 1) {
		for(i in seq(2, length(mL), by = 2)){
			mL[[i]] <- rev(mL[[i]])
		}
	}
	unlist(mL)
}


#' @export
dim.fieldPlots <- function(x){
	dim(x@matrix)
}
#' @export
print.fieldPlots <- function(x, ...){
	print(x@matrix, ...)
}