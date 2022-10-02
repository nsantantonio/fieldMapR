#' anglePoint function
#'
#' function to find new point given a reference point, an angle and a distance. 
#'
#' @param d numeric. distance traveled
#' @param theta numeric. angle in degrees to move in.
#' @param ref numeric.  Reference point to start at. length must be 2. Default is c(0, 0)
#' @return numeric. point 
#' @details shift point by angle and distance. 
#' @examples anglePoint(d = 1, theta = 0, ref = c(0,0))
#' anglePoint(d = 2, theta = 90, ref = c(1,0))
#' @export
anglePoint <- function(d, theta, ref = c(0, 0)){
	theta <- degToRad(theta)
	c(d * cos(theta), d*sin(theta)) + ref
}
