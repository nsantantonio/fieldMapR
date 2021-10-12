#' calcDist function
#'
#' function to (do something)
#'
#' @param pt1 [value]
#' @param pt2 [value]
#' @param offset [value]. Default is c(5, 10)
#' @param arlen [value]. Default is 0.10
#' @param printDist [value]. Default is TRUE
#' @param units [value]. Default is "ft"
#' @param digits [value]. Default is 0
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
calcDist <- function(pt1, pt2, offset = c(5, 10), arlen = 0.10, printDist = TRUE, units = "ft", digits = NULL){
	if(length(pt1) == 0) stop("Please provide a valid plot number for point 1")
	if(length(pt2) == 0) stop("Please provide a valid plot number for point 2")


	distance <- dist(rbind(pt1, pt2))
	angle <- radToDeg(atan2(pt2[2]-pt1[2], pt2[1]-pt1[1]))

	midpt <- (pt1 + pt2) / 2
	midptPrint <- shiftPt(midpt, a = angle, y = offset[1], x = 0)
	midptLabPrint <- shiftPt(midpt, a = angle, y = offset[2], x = 0)
	pt1Print <- shiftPt(pt1, a = angle, y = offset[1])
	pt2Print <- shiftPt(pt2, a = angle, y = offset[1])

	if(printDist){
		arrows(midptPrint[1], midptPrint[2], pt2Print[1], pt2Print[2], angle = 90, length = arlen)
		arrows(midptPrint[1], midptPrint[2], pt1Print[1], pt1Print[2], angle = 90, length = arlen)
		if(!is.null(digits)) distance <- round(distance, digits)
		text(midptLabPrint[1], midptLabPrint[2], labels = paste0(distance, " ", units), srt = angle)
	} else {
		return(distance)
	}
}
