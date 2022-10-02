#' GPStoDist function
#'
#' function to (do something)
#'
#' @param takeoutFile character. path to google takeout csv file.
#' @param rowOrder numeric. order of points. 
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
takeout <- function(takeoutFile, rowOrder = NULL, ...){
	coord <- read.csv(takeoutFile, ...)
	txtCoords <- strsplit(gsub(".*/", "", coord$URL), ",")

	if(is.null(rowOrder)) rowOrder <- 1:nrow(coord)
	boundries <- do.call(rbind, lapply(txtCoords, as.numeric))[rowOrder,c(2,1)]
	colnames(boundries) <- c("lon", "lat")
	# fieldPoly <- rbind(boundries, boundries[1,])
	return(boundries)
}