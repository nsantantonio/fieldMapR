#' haversine function
#'
#' function to (do something)
#'
#' @param lat1 [value]
#' @param lon1 [value]
#' @param lat2 [value]
#' @param lon2 [value]
#' @param r [value]. Default is 6371
#' @param units [value]. Default is "km"
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
haversine <- function(lat1, lon1, lat2, lon2, r = 6371, units = "km") {
	hav <- function(theta)  sin(theta / 2)^2
	h <- hav(degToRad(lat2 - lat1)) + cos(degToRad(lat1)) * cos(degToRad(lat2)) * hav(degToRad(lon2 - lon1))
	d <- 2 * r * asin(sqrt(h))
	names(d) <- "km"
	if (units == "ft") {
		d <- d * 3280.84
		names(d) <- "ft"
	} else if (units == "m"){
		d <- d * 1000
		names(d) <- "m"
	}
	d
}
