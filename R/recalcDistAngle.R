#' recalcDistAngle function
#'
#' function to (do something)
#'
#' @param boundry [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
recalcDistAngle <- function(boundry){
	pts <- if(is.list(boundry)) boundry@points else pts <- boundry
	j <- nrow(boundry)

	angles <- NULL
	distances <- NULL
	for(i in 1:nrow(pts)) {
		da <- pointDist(pts[j,], pts[i,])
		angles[i] <- da$a
		distances[i] <- da$d
		j <- i
	}
	fieldBoundary(points = pts, angle = angles, distance = distances)
}
