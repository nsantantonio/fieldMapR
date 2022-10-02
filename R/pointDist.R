#' pointDist function
#'
#' function to (do something)
#'
#' @param pt1 [value]
#' @param pt2 [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
pointDist <- function(pt1, pt2){
	distance <- dist(rbind(pt1, pt2))
	angle <- radToDeg(atan2(pt2[2]-pt1[2], pt2[1]-pt1[1]))
	list(d = distance, a = angle)
}
