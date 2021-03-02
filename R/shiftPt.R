#' shiftPt function
#'
#' function to (do something)
#'
#' @param pt [value]
#' @param a [value]. Default is 0
#' @param x [value]. Default is 0
#' @param y [value]. Default is 0
#' @param a2 [value]. Default is a
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
shiftPt <- function(pt, a = 0,  x = 0, y = 0, a2 = a){
	newpt <- anglePoint(d = y, a + 90, ref = pt)
	newpt <- anglePoint(d = x, a2, ref = newpt)
	newpt
}
