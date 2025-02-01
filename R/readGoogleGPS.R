#' readGoogleGPS function
#'
#' function to (do something)
#'
#' @param googleCSV [value]
#' @param ptOrder [value]. Default is NULL
#' @param ignore [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
readGoogleGPS <- function(googleCSV, ptOrder = NULL, ignore = NULL, ...){
	coord <- read.csv(googleCSV, ...)
	txtCoords <- strsplit(gsub(".*/", "", coord$URL), ",")

	if(is.null(ptOrder)) ptOrder <- 1:length(txtCoords)
	gps <- do.call(rbind, lapply(txtCoords, as.numeric))[ptOrder,c(2,1)]
	colnames(gps) <- c("lon", "lat")
	return(gps)
}
