#' makeBreedbaseTrial function to create breedbase multi trial upload files 
#'
#' function to create multi trial upload files for breedbase
#'
#' @param block object of class fieldBlock
#' @param trays data.frame with scab entries per tray.
#' @param trim numeric. Which ranges should be trimmed off of trays, typically the front (1) and last (20) ranges are planted in another species as border, so they need to be removed to format the scab block correctly.
#' @param flip default is 1
#' @param ... additional arguments passed to makeTwoListMat()
#' @return data.frame formated breedbase multi trial upload file
#' @details reformats blocks for the scab nursery
#' @examples # none.
#' @export
formatScabBlock <- function(block, trays, trim = NULL, flip = 1, ...){
	# block <- blocks[[2]]; plotNames <- block@plotName; trayNames <- plotNames[!is.na(plotNames) & !block@fill];  trays = rbind(srwTrays, hrwTrays); i = plotNames[1]; trim = c(1, 20); flipTrays = TRUE
	# block <- blocks[[1]]; plotNames <- block@plotName; trayNames <- plotNames[!is.na(plotNames) & !block@fill]; trays = barleyTrays; i = plotNames[1]; trim = c(1, 20); flipTrays = TRUE					
	makeTwoListMat <- function(block, s, trays, varName = s, matchVar = "trayName", trayNames = NULL, plotNames = NULL, traySize = 40, trim = c(1, 20), flipTrays = TRUE, flip = 1, ...){
		# s = "plotName"; varName = s; matchVar = "trayName"; traySize = 40; 
		ps <- slot(block, s)
		mat <- matrix(rep(list(), prod(dim(ps))), nrow = nrow(ps), ncol = ncol(ps))
		if(is.null(plotNames)) plotNames <- block@plotName
		if(is.null(trayNames)) trayNames <- plotNames[!is.na(plotNames) & !block@fill]
		for(i in trayNames){
			# i = trayNames[1];
			# i = "SRWscab_T1"
			whichTray <- which(plotNames == i, arr.ind = TRUE)
			if(!varName %in% names(trays)) {
				twoMat <- matrix(rep(slot(block, varName)[whichTray], traySize), ncol = 2)
			} else {
				# if(is.null(varName)) {
				# 	mat[whichTray] <- list(trays[trays[[matchVar]] == i, ])
				# } else {
				# mat[whichTray] <- list(trays[trays[[matchVar]] == i, varName])
				twoMat <- matrix(trays[trays[[matchVar]] == i, varName], ncol = 2, byrow = TRUE)
			}

			if(!is.null(trim)){
				twoMat <- twoMat[-trim, ]
			}
			mat[whichTray] <- list(twoMat[])
		}

		# }
		whichFillTrays <- which(sapply(c(mat), is.null))
		mat[whichFillTrays] <- rep(list(matrix("FILL", nrow = 20-length(trim), ncol = 2)), length(whichFillTrays))

		# make null elements as fill 
		rowL <- list()
		for (i in 1:ncol(mat)){
			if(i %% 2 == flip){ # flip forward passes back front
				for(k in 1:length(mat[, i])) {
					mat[k, i][[1]] <- mat[k, i][[1]][nrow(mat[k, i][[1]]):1, ]
				}
			}
			rowL[[i]] <- do.call(rbind, mat[, i])
		}
		fullMat <- do.call(cbind, rowL)
		if(varName %in% c("Entry", "block")){
			fullMat[grep("fill", fullMat, ignore.case = TRUE)] <- 0
			class(fullMat) <- "integer"
		}
		
		if(flipTrays){
			startFlip <- flip*2 + 1
			firstRow <- seq(startFlip, ncol(fullMat), by = 4)
			for(j in firstRow){ # flip return passes, left right
				fullMat[, c(j, j + 1)] <- fullMat[, c(j + 1, j)]
			}
		}

		return(fullMat)

	} 

	plotNames <- block@plotName
	trayNames <- plotNames[!is.na(plotNames) & !block@fill]

	# for(i in c("plotName", "plotNo", "Trial", "Entry", "Line")){
	for(i in c("plotNo", "fill", "Trial", "Entry", "Line", "plotName")){ # plotName must be done last!!
		slot(block, i) <- makeTwoListMat(block, i, trays, trayNames = trayNames, plotNames = plotNames, ...)
	}

	# fix border rows
	block@borderPasses <- sort(c(block@borderPasses * 2 - 1, block@borderPasses * 2))

	return(block)
}# }
