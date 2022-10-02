#' getGPStheta function
#'
#' function to (do something)
#'
#' @param lat1 [value]
#' @param lon1 [value]
#' @param lat2 [value]
#' @param lon2 [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
getGPStheta <- function(lat1, lon1, lat2, lon2){
	dy = lat2 - lat1
	dx = cos(degToRad(lat1)) * (lon2 - lon1)
	atan2(dy, dx)
}
