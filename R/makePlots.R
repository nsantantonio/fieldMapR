#' makePlots function
#'
#' function to make field plots. This is the primary function for this package. Produces a 
#'
#' @param trial either an obect of class 'trialDesign' or a character vector of length 1 for the trial name. 
#' @param ranges numeric. number of ranges (rows)
#' @param passes numeric. number of passes (columns)
#' @param rangeDist numeric. distance between ranges, on center. 
#' @param passDist numeric. distance between passes, on center.
#' @param rstart numeric. range to start plots on. Default is 1
#' @param pstart numeric. pass to start plots on. Default is 1
#' @param angle angle to . Default is 0
#' @param plotNo value to use for plot numbers (series, either 100, 1000 etc). or the actual plot numbers desired (length of number of plots). Default is 1000
#' @param blockSize numeric. Size of a block. Default is ranges * passes
#' @param nBlock numeric. Number of blocks. If greater than one, the series will be updated once the block is filled. I.e. if block size is 120, and there are 240 plots, then teh first block will contain plots 1001:1120 and the second block will contain plots 2001:2120. Default is 1
#' @param serpentine Should plot numbers be planted serpentine?. Default is TRUE
#' @param startSerp Should plots start numbering serpentine? Useful when the user wants to change the normal direction at ranges greater than 1. Default is FALSE
#' @param ref Reference point for all plots to start at. Typically this will be the bottom left point for teh first plot. Default is c(0,0)
#' @param border numeric. How many passes should be borders? If length is 1 than both sides will have the same number of border passes. If length is 2, then first element specifies number of left border passes and second element specifies number of right border passes. Default is 0
#' @param borderName character. What should borders be named. Default is "B"
#' @param borderPlotNo integer. What number should be used to start counting border plots? Default is 1
#' @param updateB logical.Shoud border numbers be updated? Default is TRUE
#' @param ignorePasses numeric. Which passes should be ignored when plotting? Useful when the users wants to skip passes, example, when two trials are side by side in teh same block. Default is NULL
#' @param fieldViewMatrix Should the matrix represent the field view? I.e. the first plots will be in the last rows of the matrix (i.e. the bottom). Default is TRUE
#' @param fill logical. Should the remaining plots in eh range be filled with fill?  Default is FALSE
#' @param fillBlock logical. Should the rest of the block be filled with fill? Default is FALSE
#' @param Line character. Optional vector of line names. Default is NULL
#' @param Pedigree character. Optional vector of line pedigrees. Default is NULL
#' @param Entry integer. Optional vector of Entry numbers names. Default is NULL
#' @return object of class 'fieldPlots'
#' @details [fill in details here]
#' @examples # none
#' @export
# trial = "test"; ranges = 20; passes = 30; rangeDist = 16; passDist = 5; rstart = 1; pstart = 1; angle = 0; plotNo = 1000; blockSize = 300; nBlock = 2; serpentine = TRUE; startSerp = FALSE; transpose = FALSE; ref = c(0,0); border = 0; borderName = "B"; borderPlotNo = 1; updateB = TRUE; ignorePasses = NULL; fieldViewMatrix = TRUE; fill = FALSE; fillBlock = FALSE; Line = NULL; Pedigree = NULL; Entry = NULL; Rep = NULL
makePlots <- function(trial, ranges, passes, rangeDist = 16, passDist = 5, rstart = 1, pstart = 1, angle = 0, plotNo = 1000, blockSize = ranges * passes, nBlock = 1, serpentine = TRUE, startSerp = FALSE, ref = c(0,0), border = 0, borderName = "B", borderPlotNo = 1, updateB = TRUE, ignorePasses = NULL, fieldViewMatrix = TRUE, fill = FALSE, fillBlock = FALSE, Line = NULL, Pedigree = NULL, Entry = NULL, Rep = NULL){
# trial = "ExtraObs"; ranges = ranges1; passes = 36; ref = shiftPt(startPt, x = -(40 + 38*5)); angle = a; pstart = P; rstart = R; plotNo = paste0("xObs", 1:5); border = c(1,1); borderName = "W"; fill = TRUE
	if (class(trial) %in% "trialDesign"){
		if(!all(order(trial@plotNo) == 1:length(trial@plotNo))){
			stop("Plots out of order! Please order plots in ascending order!")
		}
		# blockSize <- unique(table(trial@plotInfo[["Block"]])) # these need to be from the trial@block, not from plotInfo
		# nBlock <- length(unique(trial@plotInfo[["Block"]]))
		Rep <- trial@block # change to Rep for consistency?
		blockSize <- unique(table(trial@block))
		nBlock <- length(unique(trial@block))
		lastPlot <- max(trial@plotNo)
		plotName <- trial@plotName
		# if(length(blockSize) > 1 | length(1:lastPlot) == length(trial@plotName)){
		plotNoGiven <- TRUE
		plotNo <- trial@plotNo
		# } else { # a plot number must be provided for trialDesign!!!!
		# 	plotNoGiven <- FALSE
		# 	plotNo <- min(trial@plotNo) - 1
		# }
		trialName <- unique(trial@trialName)
		if(length(trialName) > 1) stop("More than one trial in plots!")
		Entry <- trial@Entry
		Line <- trial@Line
		Pedigree <- trial@Pedigree
		# or should I try to use S4 throughout? convert args to s4 trialDesign?
	} else if (is.character(trial) & length(trial) == 1) {
		trialName <- trial
		if(is.null(Rep)) class(Rep) <- "integer"
		if(is.null(Entry)) class(Entry) <- "integer"
		if(is.null(Line)) class(Line) <- "character"
		if(is.null(Pedigree)) class(Pedigree) <- "character"
		if (length(plotNo) > 1 | is.character(plotNo) | {length(plotNo) == 1 & plotNo[[1]] == 1}) {
			plotNoGiven <- TRUE
		} else {
			plotNoGiven <- FALSE
		}

	} else {
		stop("'trial' must be either a object of class 'trialDesign' or a character vector of length 1 with the trial name")
	}

	if(length(border) == 1) {
		border <- c(border, border)
	} else if(length(border) > 2){
		stop("border argument only accepts a vector of length 1 or 2, when length 2, specfies number of left and right border passes, respectively")	
	}

	whichRev <- if(startSerp) 1 else 0 
	isSerp <- if(rstart %% 2 == whichRev) TRUE else FALSE

	if(plotNoGiven) {
		plotIndex <- 1
		allPlotNo <- plotNo
		plotNo <- allPlotNo[1]
		lastPlot <- allPlotNo[length(allPlotNo)]
		if(length(Rep) == 0){ 
			message("Nothing provided to Rep argument, assuming single replicate.")
			Rep <- rep(1, length(allPlotNo))
		}
	} else {
		maxN <- ranges * passes
		if(sum(blockSize) > maxN) {
			warning("block size cannot exceed the number of plots!")
			blockSize <- rep(maxN, nBlock)
		}
		series <- plotNo
		step <- 10^floor(log10(plotNo))
		plotNo <- plotNo + 1
		lastPlot <- nBlock * series + blockSize 
		# maxB <- floor(maxN / blockSize)
		# lastPlot <- nBlock * series + blockSize 
	}

	print(paste0("Last Plot: ", lastPlot))

	if(rstart > ranges) stop("rstart cannot exceed number of ranges!")

	pdim <- c(passDist, rangeDist)

	# otherWay <- pstart == 1 & isSerp
	# inMiddle <- if(pstart > 1 | otherWay) TRUE else FALSE
	inMiddle <- if(pstart > 1) TRUE else FALSE

	passes <- passes + sum(border)
	# inMiddle <- if({pstart > 1 & !isSerp} | {pstart < passes & pstart > border[1] & isSerp}) TRUE else FALSE

	# borderPlotNo <- 1	

	borderIndex <- list(if(border[1] > 0) {0:(passes-1)}[1:border[1]] else NULL, if(border[2] > 0) {(passes-1):0}[1:border[2]] else NULL)
	borderj <- unlist(borderIndex)
	# print(borderj)

	plotNameij <- matrix(NA, ranges, passes)
	fillij <- matrix(NA, ranges, passes)
	repij <- matrix(NA, ranges, passes)
	entij <- matrix(NA, ranges, passes)
	lineij <- matrix(NA, ranges, passes)
	pedij <- matrix(NA, ranges, passes)
 
	isfill <- FALSE
	tooFar <- FALSE
	saveRP <- TRUE
	isLast <- if(sum(blockSize) > 0) FALSE else TRUE
	printLast <- FALSE
	blockSwitch <- TRUE
	entryIndex <- 1

	if(length(ignorePasses)) ignorePasses <- ignorePasses + border[1]

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
				# if(fill | isSerp & j == (border[1] + 1) | !isSerp & j == (passes - border[2]- 1)) { 
				# if(fill | isSerp & j == (border[1] + 1) | !isSerp & j == (passes - border[2])) { # commented out Nov 4 2021
				if(fill | isSerp & j == (border[1] - 1) | !isSerp & j == (passes - border[2])) { 
					tooFar <- TRUE
					if(saveRP){
						R <<- i + 2
						R <- i + 2
						P <<- 1
						P <- 1
						saveRP <- FALSE
					}
				} else {
					if(saveRP){
						R <<- i + 1
						R <- i + 1
						if(isSerp) {
							P <<- j + 2 - sum(border)  # I think this is right. I still dont understand why +2...
							P <- j + 2 - sum(border)  
						} else {
							P <<- j + 1 - border[1]
							P <- j + 1 - border[1]
						}
						saveRP <- FALSE
					}
					break
				}
			} else {
				if (plotNo == lastPlot) {
					isLast <- TRUE
				}
			}
			if(length(ignorePasses)){
				if(j %in% (ignorePasses - 1)) next
			}

			if(inMiddle){
				shouldSkip <- if (isSerp & i == rstart-1){
					j >= pstart - 1 + sum(border) # this is original and worked fine before. 
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
				fillij[i + 1, j + 1] <- TRUE
				borderPlotNo <- borderPlotNo + 1
			} else {
				if(isLast) printLast <- TRUE

				plotCorners[[as.character(plotNo)]] <- corners
				plotCenters[[as.character(plotNo)]] <- c(j + 0.5, i + 0.5) * pdim
				plotNameij[i + 1, j + 1] <- as.character(plotNo) 
				fillij[i + 1, j + 1] <- FALSE
				if(plotNoGiven) repij[i + 1, j + 1] <- Rep[entryIndex] else repij[i + 1, j + 1] <- as.integer(substr(as.character(plotNo), 1, 1))
				entij[i + 1, j + 1] <- Entry[entryIndex]
				lineij[i + 1, j + 1] <- Line[entryIndex]
				pedij[i + 1, j + 1] <- Pedigree[entryIndex]
				entryIndex <- entryIndex + 1

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
							# break
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
	if(updateB) {
		B <<- borderPlotNo
		B <- borderPlotNo
	}

	plotCorners <- lapply(plotCorners, function(x) do.call(rbind, x))
	if (angle != 0){
		allCorners <- do.call(rbind, plotCorners)
		allCornersRot <- rotate(allCorners, angle)
		plotCornersRot <- lapply(split(allCornersRot, rep(1:length(plotCorners), each = 4)), matrix, ncol = 2)
		names(plotCornersRot) <- names(plotCorners)
		centersRot <- split(rotate(do.call(rbind, plotCenters), angle), 1:length(plotCenters))
		names(centersRot) <- names(plotCenters)
		# plots <- fieldPlots(centers = centersRot, corners = plotCornersRot, matrix = plotNameij, fill = fillij, needStake = blockChange, Entry = Entry, Line = Line, Pedigree = Pedigree)
		plots <- fieldPlots(centers = centersRot, corners = plotCornersRot, matrix = plotNameij, fill = fillij, needStake = blockChange, Rep = repij, Entry = entij, Line = lineij, Pedigree = pedij)
	} else {
		# plots <- fieldPlots(centers = plotCenters, corners = plotCorners, matrix = plotNameij, fill = fillij, needStake = blockChange, Entry = Entry, Line = Line, Pedigree = Pedigree)
		plots <- fieldPlots(centers = plotCenters, corners = plotCorners, matrix = plotNameij, fill = fillij, needStake = blockChange, Rep = repij, Entry = entij, Line = lineij, Pedigree = pedij)
	}
	if(any(ref != 0)){
		plots@centers <- lapply(plots@centers, function(x) x + ref)
		plots@corners <- lapply(plots@corners, function(x) sweep(x, 2, ref, "+"))
	}

	if(fieldViewMatrix) {
		plots@matrix <- plots@matrix[nrow(plots@matrix):1, , drop = FALSE]
		plots@fill <- plots@fill[nrow(plots@fill):1, , drop = FALSE]
		plots@Rep <- plots@Rep[nrow(plots@Rep):1, , drop = FALSE]
		plots@Entry <- plots@Entry[nrow(plots@Entry):1, , drop = FALSE]
		plots@Line <- plots@Line[nrow(plots@Line):1, , drop = FALSE]
		plots@Pedigree <- plots@Pedigree[nrow(plots@Pedigree):1, , drop = FALSE]
	}
	# plots <- c(plots, list(matrix = plotNameij))
	if(is.null(borderj)) {
		borderj <- NA
		class(borderj) <- "numeric"
		plots@borderPasses <- borderj
	} else {
		plots@borderPasses <- borderj + 1
	}
	plots@trialName <- trialName
	if(!isLast){
		warning("Plots ended before end of trial!")
	}
	return(plots)
}
