#' makeSquareField function
#'
#' function to fix planting mistakes in the field
#'
#' @param w, numeric. width of field in feet
#' @param l, length of field in feet
#' @param refx, x coordinate to start at. Default is 0
#' @param refy, x coordinate to start at. Default is 0
#' @return object of class 'fieldBoundary'
#' @details [fill in details here]
#' @examples # none
#' @export
makeSquareField <- function(w, l, refx = 0, refy = 0) {
	pointmat <- matrix(c(refx, refx + w, refx + w, refx, refy, refy, refy + l, refy + l), ncol = 2)
	field <- matrixToDist(pointmat) 
	return(field)
}