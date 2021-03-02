#' plotGPS function
#'
#' function to (do something)
#'
#' @param gps [value]
#' @param number [value]. Default is TRUE
#' @param a [value]. Default is 0
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
plotGPS <- function(gps, number = TRUE, a = 0){
	if (a != 0) {
		gps <- rotate(gps, a)
	}
	plot(gps, type = "n", asp = 1)
	lines(rbind(gps, gps[1,]))
	text(gps, labels = 1:nrow(gps))
}
