#' transLatLon function
#'
#' function to (do something)
#'
#' @param lat1 [value]
#' @param lon1 [value]
#' @param lat2 [value]
#' @param lon2 [value]
#' @param ref [value]
#' @param units [value]. Default is "ft"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
transLatLon <- function(lat1, lon1, lat2, lon2, ref, units = "ft") {
	d <- haversine(lat1, lon1, lat2, lon2, units = units)
	theta <- getGPStheta(lat1, lon1, lat2, lon2)
	list(point = c(d*cos(theta), d*sin(theta)) + ref, angle = radToDeg(theta), distance = d)
}
