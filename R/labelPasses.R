#' labelPasses function
#'
#' function to label ranges using fieldBlocks. 
#'
#' @param blockList [value]
#' @param startPt [value]. Default is "ft"
#' @param lastPass [value]. Default is NULL
#' @param passDist [value]. Default is NULL
#' @param ... additional arguments passed to text()
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export

labelPasses <- function(blockList, startPt = c(0,0), lastPass = 0, passDist = 5, ...){
	if(class(blockList) == "fieldBlock") blockList <- list(blockList)
	npasses <- sum(sapply(blockList, ncol))
	xpos <- startPt[1] + (1:npasses - 0.5) * passDist 
	text(x = xpos, y = startPt[2], labels = {lastPass+1}:{npasses+lastPass}, ...)
}
