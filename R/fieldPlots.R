#' An S4 class to represent field plots
#' @name fieldPlots
#' @slot centers  list of centers
#' @slot corners  list of corners
#' @slot matrix  matrix of matrix
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot trialName  character of trialName
#' @export fieldPlots
fieldPlots <- setClass("fieldPlots", slots = c(centers = "list", corners = "list", matrix = "matrix", needStake = "list", borderPasses = "numeric", trialName = "character"))

#` rbind method for fieldplots-class
#' @rdname fieldPlots
#' @include bind.R
setMethod("rbind", "fieldPlots", function(...){
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
	})

#` cbind method for fieldplots
#' @rdname fieldPlots
#' @include bind.R
setMethod("cbind", "fieldPlots", function(...){
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
	})




# t1 <- makePlots(trialName = "test1", ranges = 5, passes = 3, ref = c(0,0), angle = a, 
# 				   rangeDist = 16, passDist = 5, borderName = "B",
# 				   pstart = 1, rstart = 1, blockSize = 5, nBlock = 3,
# 				   plotNo = 100, border = c(1, 0), fill = TRUE)

# t2 <- makePlots(trialName = "test1", ranges = 5, passes = 4, ref = c(0,0), angle = a, 
# 				   rangeDist = 16, passDist = 5, borderName = "B",
# 				   pstart = 1, rstart = 1, blockSize = 4, nBlock = 5,
# 				   plotNo = 100, border = c(1, 0), fill = TRUE)

# t3 <- makePlots(trialName = "test1", ranges = 5, passes = 2, ref = c(0,0), angle = a, 
# 				   rangeDist = 16, passDist = 5, borderName = "B",
# 				   pstart = 1, rstart = 1, blockSize = 5, nBlock = 2,
# 				   plotNo = 100, border = c(0, 0), fill = TRUE)

# t4 <- makePlots(trialName = "test1", ranges = 5, passes = 3, ref = c(0,0), angle = a, 
# 				   rangeDist = 16, passDist = 5, borderName = "B",
# 				   pstart = 1, rstart = 1, blockSize = 3, nBlock = 5,
# 				   plotNo = 100, border = c(1, 1), fill = TRUE)
# l <- list(t1, t2, t3, t4)

