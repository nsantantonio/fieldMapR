#' anglePoint function
#'
#' function to (do something)
#'
#' @param d [value]
#' @param theta [value]
#' @param ref [value]. Default is c(0, 0)
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
anglePoint <- function(d, theta, ref = c(0, 0)){
	theta <- degToRad(theta)
	c(d * cos(theta), d*sin(theta)) + ref
}
