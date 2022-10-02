#' fixPlantingMistakes function
#'
#' function to fix planting mistakes in the field
#'
#' @param plots object of class 'fieldPlots' or 'fieldBlock'
#' @param swap a list with each element of length 2, or matrix with two columns, where the first element or column is the plotName of the seed source that was planted into the space originally intended to be occupied by the plotName in the second element or column
#' @return [value ] object of class 'fieldPlots' or 'fieldBlock' corresponding to 'plots' argument, with the Trial, Entry and Line swapped out. 
#' @details [fill in details here]
#' @examples # none
#' @export
fixPlantingErrors <- function(plots, swap) {

	swapMat <- function(plots, swap, slots){
		if("plotName" %in% slotNames(plots)) {
			plotName <- plots@plotName
		} else {
			plotName <- matrix(paste0(plots@trialName, "_", plots@matrix), nrow = nrow(plots@matrix), ncol = ncol(plots@matrix))
		}
		plotIndex <- list()
		seedIndex <- list()
		for(i in 1:nrow(swap)){
			plotIndex[[swap[i,2]]] <- which(plotName == swap[i,2], arr.ind = TRUE)
			seedIndex[[swap[i,1]]] <- which(plotName == swap[i,1], arr.ind = TRUE)
		}
		for(i in slots){
			mat <- slot(plots, i)
			new <- list()
			for(j in 1:length(plotIndex)){
				new[[names(plotIndex)[j]]] <- mat[seedIndex[[j]]]
			}
			for(j in 1:length(new)){
				mat[plotIndex[[j]]] <- new[[j]]
			}
			slot(plots, i) <- mat
		}
		return(plots)
	}

	if(class(swap) == "list") {
		if(!all(sapply(swap, length) == 2)) stop("All list elements must be of length 2, with the first element indicating the plot seed, and the second indicating the plot it was planted into.")
		swap <- do.call(rbind, swap)
	}

	swapplots <- c(swap)
	
	# plotName <- matrix(paste0(plots@trialName, "_", plots@matrix), nrow = nrow(plots@matrix), ncol = ncol(plots@matrix))


	if(any(!swapplots %in% plots@plotName)){
		warning("the following plot names in 'swap' are not in the 'plots'!")
		print(swapplots[!swapplots %in% plots@plotName])
	}
	
	plots <- swapMat(plots, swap, c("Trial", "Line", "Entry"))
	
	if(class(plots) == "fieldBlock"){
		long <- plots@long

		plotIndexl <- list()
		seedIndexl <- list()
		for(i in 1:nrow(swap)){
			plotIndexl[[swap[i,2]]] <- which(long$plotName == swap[i,2], arr.ind = TRUE)
			seedIndexl[[swap[i,1]]] <- which(long$plotName == swap[i,1], arr.ind = TRUE)
		}

		long 
		vars <- c("Trial", "Line", "Entry")
		new <- list()
		for(j in 1:length(plotIndexl)){
			new[[names(plotIndexl)[j]]] <- long[seedIndexl[[j]], vars]
		}
		for(j in 1:length(new)){
			long[plotIndexl[[j]], vars] <- new[[j]]
		}
		slot(plots, "long") <- long
	}
	return(plots)
}