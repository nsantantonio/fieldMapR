#' makePlots function
#'
#' function to (do something)
#'
#' @param trialName [value]
#' @param ranges [value]
#' @param passes [value]
#' @param rangeDist [value]
#' @param passDist [value]
#' @param rstart [value]. Default is 1
#' @param pstart [value]. Default is 1
#' @param angle [value]. Default is 0
#' @param plotNo [value]. Default is 1000
#' @param blockSize [value]. Default is ranges * passes
#' @param nBlock [value]. Default is 1
#' @param serpentine [value]. Default is TRUE
#' @param startSerp [value]. Default is FALSE
#' @param transpose [value]. Default is FALSE
#' @param ref [value]. Default is c(0,0)
#' @param border [value]. Default is 0
#' @param borderName [value]. Default is "B"
#' @param borderPlotNo [value]. Default is 1
#' @param updateB [value]. Default is TRUE
#' @param ignorePasses [value]. Default is NULL
#' @param fieldViewMatrix [value]. Default is TRUE
#' @param fill [value]. Default is FALSE
#' @param fillBlock [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export

makePlots <- function(trialName, ranges, passes, rangeDist, passDist, rstart = 1, pstart = 1, angle = 0, plotNo = 1000, blockSize = ranges * passes, nBlock = 1, serpentine = TRUE, startSerp = FALSE, transpose = FALSE, ref = c(0,0), border = 0, borderName = "B", borderPlotNo = 1, updateB = TRUE, ignorePasses = NULL, fieldViewMatrix = TRUE, fill = FALSE, fillBlock = FALSE){
	if(length(border) == 1) {
		border <- c(border, border)
	} else if(length(border) > 2){
		stop("border argument only accepts a vector of length 1 or 2, when length 2, specfies number of left and right border passes, respectively")	
	}

	whichRev <- if(startSerp) 1 else 0 
	isSerp <- if(rstart %% 2 == whichRev) TRUE else FALSE

	plotNoGiven <- if (length(plotNo) > 1 | is.character(plotNo)) TRUE else FALSE

	if(plotNoGiven) {
		plotIndex <- 1
		allPlotNo <- plotNo
		plotNo <- allPlotNo[1]
		lastPlot <- allPlotNo[length(allPlotNo)]
	} else {
		if(blockSize > ranges * passes) {
			warning("block size cannot exceed the number of plots!")
			blockSize <- ranges * passes
		}
		series <- plotNo
		step <- plotNo
		plotNo <- plotNo + 1
		maxN <- ranges * passes
		maxB <- floor(maxN / blockSize)
		lastPlot <- nBlock * series + blockSize 
	}

	print(paste0("Last Plot: ", lastPlot))

	if(rstart > ranges) stop("rstart cannot exceed number of ranges!")

	pdim <- c(passDist, rangeDist)

	inMiddle <- if(pstart > 1) TRUE else FALSE

	passes <- passes + sum(border)

	# borderPlotNo <- 1	

	borderIndex <- list(if(border[1] > 0) {0:(passes-1)}[1:border[1]] else NULL, if(border[2] > 0) {(passes-1):0}[1:border[2]] else NULL)
	borderj <- unlist(borderIndex)
	# print(borderj)

	plotNameij <- matrix(NA, ranges, passes)
 
	isfill <- FALSE
	tooFar <- FALSE
	saveRP <- TRUE
	isLast <- if(blockSize > 0) FALSE else TRUE
	printLast <- FALSE
	blockSwitch <- TRUE

	blockChange <- list()
	plotCorners <- list()
	plotCenters <- list()

	for (i in (rstart - 1):(ranges-1)){
		rangeDir <- if (serpentine & isSerp) (passes-1):0 else 0:(passes-1)
		for (j in rangeDir){

			if(blockSwitch & !j %in% borderj & !printLast){
				blockChange <- c(blockChange, list(c(flipInt(i + 1, ranges), j + 1)))
				# blockChange <- c(blockChange, list(c(i, j) + 1))
				blockSwitch <- FALSE
			}
			if (isLast & printLast) {
				# if(fill | j == (ranges - 1)) { 
				# if(fill | isSerp & j == (border[1] + 1) | !isSerp & j == (passes - border[2]- 1)) { 
				if(fill | isSerp & j == (border[1] + 1) | !isSerp & j == (passes - border[2])) { 
					tooFar <- TRUE
					if(saveRP){

						R <<- i + 2
						P <<- 1
						saveRP <- FALSE
					}
				} else {
					if(saveRP){
						# blockChange <- c(blockChange, list(c(flipInt(i + 1, ranges), j + 1)))
						# blockSwitch <- FALSE
						R <<- i + 1
						if(isSerp) {
							# P <<- j + 2 + border[2]
							# P <<- j - 1 - border[2]
							P <<- j + 2 - sum(border)  # I think this is right. I still dont understand why +2...
						} else {
							P <<- j + 1 - border[1]
						}
						saveRP <- FALSE
					}
					break
				}
			} else {
				if (plotNo == lastPlot) isLast <- TRUE
			}
			if(length(ignorePasses)){
				if(j %in% (ignorePasses - 1)) next
			}

			if(inMiddle){
				shouldSkip <- if (isSerp & i == rstart-1){
					j >= pstart - 1 + sum(border) # this is original and worked fine before. 
					# j >= pstart + 1 - sum(border)
				} else {
					j < pstart - 1 + border[1]
				}
				if (shouldSkip & i <= rstart - 1) next
				if(i == rstart) inMiddle <- FALSE
			}

			corners <- list(c(j, i) * pdim,
							c(j+1, i) * pdim,
							c(j+1, i+1) * pdim,
							c(j, i+1) * pdim)


			if(j %in% borderj | tooFar | isfill){
				plotCorners[[paste0(borderName, as.character(borderPlotNo))]] <- corners
				plotCenters[[paste0(borderName, as.character(borderPlotNo))]] <- c(j + 0.5, i + 0.5) * pdim
				plotNameij[i + 1, j + 1] <- paste0(borderName, borderPlotNo)
				borderPlotNo <- borderPlotNo + 1
			} else {
				if(isLast) printLast <- TRUE

				plotCorners[[as.character(plotNo)]] <- corners
				plotCenters[[as.character(plotNo)]] <- c(j + 0.5, i + 0.5) * pdim
				plotNameij[i + 1, j + 1] <- as.character(plotNo) 

				if (plotNoGiven) {
					plotIndex <- plotIndex + 1
					# print(plotIndex)
					if(plotIndex <= length(allPlotNo)) plotNo <- allPlotNo[plotIndex]
					if(plotIndex == length(allPlotNo)){
						blockChange <- c(blockChange, list(c(flipInt(i + 1, ranges), j + 1)))
						blockSwitch <- FALSE
					}
				} else {
					if(plotNo != borderName){
						if(plotNo == series + blockSize){
							blockChange <- c(blockChange, list(c(flipInt(i + 1, ranges), j + 1)))
							blockSwitch <- TRUE
							series <- series + step
							plotNo <- series + 1
							if(fillBlock) isfill <- TRUE
						} else {
							plotNo <- plotNo + 1
						}			
					}
				}
			}
		}
		if (fillBlock) isfill <- FALSE
		if (tooFar) break
		if (serpentine) isSerp <- !isSerp
	}
	# blockChange <- c(blockChange, list(c(flipInt(i + 1, ranges), j + 1)))
	# blockChange <- c(blockChange, list(c(i, j) + 1))
	if(updateB) B <<- borderPlotNo

	plotCorners <- lapply(plotCorners, function(x) do.call(rbind, x))
	if (angle != 0){
		allCorners <- do.call(rbind, plotCorners)
		allCornersRot <- rotate(allCorners, angle)
		plotCornersRot <- lapply(split(allCornersRot, rep(1:length(plotCorners), each = 4)), matrix, ncol = 2)
		names(plotCornersRot) <- names(plotCorners)
		centersRot <- split(rotate(do.call(rbind, plotCenters), angle), 1:length(plotCenters))
		names(centersRot) <- names(plotCenters)
		plots <- fieldPlots(centers = centersRot, corners = plotCornersRot, matrix = plotNameij, needStake = blockChange)
	} else {
		plots <- fieldPlots(centers = plotCenters, corners = plotCorners, matrix = plotNameij, needStake = blockChange)
	}
	if(any(ref != 0)){
		plots@centers <- lapply(plots@centers, function(x) x + ref)
		plots@corners <- lapply(plots@corners, function(x) sweep(x, 2, ref, "+"))
	}

	if(fieldViewMatrix) plots@matrix <- plots@matrix[nrow(plots@matrix):1, , drop = FALSE]
	# plots <- c(plots, list(matrix = plotNameij))
	if(is.null(borderj)) {
		borderj <- NA
		class(borderj) <- "numeric"
		plots@borderPasses <- borderj
	} else {
		plots@borderPasses <- borderj + 1
	}
	plots@trialName <- trialName
	return(plots)
}
