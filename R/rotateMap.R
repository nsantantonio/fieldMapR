#' rotateMap function
#'
#' function to (do something)
#'
#' @param map [value]
#' @param theta [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
rotateMap <- function(map, theta){
	map@points <- rotate(map@points, theta)
	boundRef <- apply(map@points, 2, min)
	map@points <- sweep(map@points, 2, boundRef, "-")
	map@angle <- map@angle + theta
	map
}
