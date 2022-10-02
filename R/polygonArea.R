#' polygonArea function
#'
#' function to (do something)
#'
#' @param coords [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
polygonArea <- function(coords){
	if(class(coords) != "matrix" | ncol(coords) != 2) stop("please provide a two column matrix of coordinates, x & y")
	coords <- unique(coords)
	x <- coords[,1]
	y <- coords[,2]

	area <- NULL
	j <- length(x) 

	for(i in 1:length(x)){
		area[i] <- (x[j]*y[i]) - (x[i]*y[j])
		j <- i
	}
	abs(sum(area) / 2)
}
