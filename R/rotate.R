#' rotate function
#'
#' function to (do something)
#'
#' @param X [value]
#' @param theta [value]
#' @param deg [value]. Default is TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
rotate <- function(X, theta, deg = TRUE){
	if (deg) theta <- theta * pi / 180
	if(theta != 0) {
		R <- matrix(c(cos(theta), sin(theta), -sin(theta), cos(theta)), 2, 2)
		return(X %*% t(R))
	} else {
		return(X)
	}
}
