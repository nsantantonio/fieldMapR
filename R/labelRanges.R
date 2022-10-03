#' labelRanges function
#'
#' function to label ranges using fieldBlocks. 
#'
#' @param blockList [value]
#' @param startPt [value]. Default is "ft"
#' @param lastRange [value]. Default is NULL
#' @param rangeDist [value]. Default is NULL
#' @param ... additional arguments passed to text()
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export

labelRanges <- function(blockList, startPt = c(0,0), lastRange = 0, rangeDist = 16, ...){
	if(class(blockList) == "fieldBlock") blockList <- list(blockList)
	nranges <- max(sapply(blockList, nrow))
	ypos <- startPt[2] + ({lastRange+1}:nranges - 0.5) * rangeDist 
	text(x = startPt[1], y = ypos, labels = {lastRange+1}:{nranges}, ...)
}
