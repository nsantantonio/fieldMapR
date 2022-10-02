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
	newL <- list()
	newL$points <- Reduce(rbind, lapply(l, "[[", "points"))
	newL$angle <- Reduce(c, lapply(l, "[[", "angle"))
	newL$distance <- Reduce(c, lapply(l, "[[", "distance"))

	rot <- rotateMap(newL, a)
	spltby <- rep(1:length(l), times = sapply(l, function(x) length(x$distance)))

	spltRot <- lapply(rot, split, spltby)

	rotL <- list()
	for(i in 1:length(l)){
		rotL[[i]]<- lapply(spltRot, "[[", i)
		rotL[[i]][["points"]] <- matrix(rotL[[i]][["points"]], ncol = 2)
	}

	rotL
}
