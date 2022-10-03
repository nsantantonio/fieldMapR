#' plotStartAngle function
#'
#' function to (do something)
#'
#' @param refCorner reference corner.
#' @param startPt [value]
#' @param pt1 [value]
#' @param pt2 [value]
#' @param refAngle1 [value]
#' @param refAngle2 [value]
#' @param rscale [value]. Default is 0.7
#' @param offset [value]. Default is 4
#' @param txtSize [value]. Default is 1
#' @param units [value]. Default is "ft"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
plotStartAngle <- function(startPt, pt1, pt2, refAngle1, refAngle2, refCorner, rscale = 0.7, offset = 4, txtSize = 1, units = "ft"){

	circ <- pointDist(refCorner, startPt)

	lines(rbind(pt1, pt2))

	x <- seq(degToRad(refAngle1), degToRad(refAngle2), length.out = 200)

	lines(circ$d * cos(x) + pt1[1], circ$d * sin(x) + pt1[2])

	halfd <- circ$d * rscale
	a1 <- mean(c(refAngle1, circ$a))
	a2 <- mean(c(refAngle2, circ$a))
	
	ppt1 <- anglePoint(d = halfd, theta = a1, ref = pt1)
	text(ppt1[1], ppt1[2], labels = paste0(round(circ$a - refAngle1, 1), "\u00B0"), cex = txtSize)

	ppt2 <- anglePoint(d = halfd, theta = a2, pt1)
	text(ppt2[1], ppt2[2], labels = paste0(round(refAngle2 - circ$a, 1), "\u00B0"), cex = txtSize)

	dpt <- anglePoint(d = halfd, theta = circ$a, ref = pt1)
	if(circ$a > 90 & circ$a < 270) {
		dang <- circ$a + 180
		offset <- -offset 
	} else {
		dang <- circ$a
	}

	dptp <- shiftPt(dpt, circ$a, y = offset)
	text(dptp[1], dptp[2], labels = paste0(round(circ$d, 1), " ", units), srt = dang, cex = txtSize)
}
