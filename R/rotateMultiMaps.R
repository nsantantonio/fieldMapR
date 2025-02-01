#' rotateMultiMaps function
#'
#' function to (do something)
#'
#' @param l [value]
#' @param a [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
rotateMultiMaps <- function(l, a){
	# l = list(boundryFt, boFup); a = rotateWholeMap
	newRef <- unique(lapply(l, slot, "reference"))
	if(length(newRef) != 1) {
		stop("All maps must have the same reference! Use refPoint argument in the GPStoDist() function to set them all the same")
	} else {
		newRef <- newRef[[1]]
	}
	newPoints <- do.call(rbind, lapply(l, slot, "points"))
	newAngle <- unlist(lapply(l, slot, "angle"))
	newDistance <- unlist(lapply(l, slot, "distance"))

	rot <- rotateMap(fieldBoundary(points = newPoints, angle = newAngle, distance = newDistance, reference = newRef), a)
	spltby <- rep(1:length(l), times = sapply(l, function(x) length(x@distance)))

	rotL <- list()
	for(i in 1:length(l)){
		isl <- spltby == i
		rotL[[i]] <- fieldBoundary(points = rot@points[isl, ], angle = rot@angle[isl], distance = rot@distance[isl], reference = rot@reference)
	}
	
	return(rotL)
}
