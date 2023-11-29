#' calcDist function
#'
#' function to plot the interval between two points on the field map and display the distance between them. 
#'
#' @param pt1 numeric. point 1, must be length 2.
#' @param pt2 numeric. point 2, must be length 2.
#' @param offset numeric. Offset to display calculated value. Default is c(5, 10)
#' @param arlen numeric. length of tick marks. Default is 0.10
#' @param printDist logical. Should the distance be printed on the map? Default is TRUE
#' @param units character. units used for measurment. Not functional only displayed as units on printed value. Default is "ft"
#' @param digits integer. number of digits to display in printed distance value. Default is 0
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @importFrom stats dist
#' @export
calcDist <- function(pt1, pt2, offset = c(5, 10), arlen = 0.10, printDist = TRUE, units = "ft", digits = 2){
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
