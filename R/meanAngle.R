#' meanAngle function
#'
#' function to (do something)
#'
#' @param a1 [value]
#' @param a2 [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
meanAngle <- function(a1, a2){
	if(a1 < 0) a1 <- a1 + 360
	if(a2 < 0) a2 <- a2 + 360
	mean(c(a1, a2))
}
