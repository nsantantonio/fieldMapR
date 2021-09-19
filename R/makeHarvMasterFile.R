#' makeHarvMasterFile function
#'
#' function to (do something)
#'
#' @param block[value] Object of class 'fieldBlock'
#' @param printPlotNo [value]. Should plot number be printed? Default is TRUE
#' @param rmBorderRanges [value]. Should all border ranges be removed? Default is FALSE
#' @param rmBorderRanges [value]. Should all border passes be removed? Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export

makeHarvMasterFile <- function(block, printPlotNo = TRUE, rmBorderPasses = TRUE, rmBorderRanges = FALSE){
	# block <- blocks[[1]]
	block@long[["plotName"]] <- paste0(block@long[["trial"]], "_", block@long[["plotNo"]])
	harvM <- block@long[c("plotName", "range", "pass", "trial")]
	names(harvM) <- c("Plot ID", "Range", "Row", "Study")
	if(rmBorderPasses) {
		harvM <- harvM[!harvM$Row %in% block@borderPasses,]
		shiftRow <- min(harvM$Row) - 1
		harvM$Row <- harvM$Row - shiftRow
	}
	if(rmBorderRanges){
		isBorder <- gsub(".*_[0-9]*", "", harvM[["Plot ID"]]) != "" # relies on border plots starting with letter, others integer
		harvM <- harvM[!isBorder,]
		shiftRange <- min(harvM$Range) - 1
		harvM$Range <- harvM$Range - shiftRange
	}

	if(printPlotNo) harvM$plotNo <- gsub(".*_", "", harvM[["Plot ID"]])
	harvM
}
