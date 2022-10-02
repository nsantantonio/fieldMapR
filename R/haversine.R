#' haversine function
#'
#' function to compute haversine distance based on two GPS coordinates. 
#'
#' @param lat1 numeric. latitude point 1.
#' @param lon1 numeric. longitude point 1.
#' @param lat2 numeric. latitude point 2.
#' @param lon2 numeric. longitude point 2.
#' @param r [value]. Default is 6371
#' @param units units to return. values can be 'km' for kilometers, 'm' for meters, 'mi' for miles, or 'ft' for feet. Default is "km"
#' @return [value]
#' @details [fill in details here]
#' @examples haversine(32.301087, -106.747767, 42.381671, -76.382452) # distance traveled from one grad school to another, in km
#' @export
haversine <- function(lat1, lon1, lat2, lon2, r = 6371, units = "km") {
	hav <- function(theta)  sin(theta / 2)^2
	h <- hav(degToRad(lat2 - lat1)) + cos(degToRad(lat1)) * cos(degToRad(lat2)) * hav(degToRad(lon2 - lon1))
	d <- 2 * r * asin(sqrt(h))
	names(d) <- "km"
	if (units == "ft") {
		d <- d * 3280.84
		names(d) <- "ft"
	} else if (units == "mi"){
		d <- d * 0.621371
		names(d) <- "mi"
	} else if (units == "m"){
		d <- d * 1000
		names(d) <- "m"
	}
	d
}
