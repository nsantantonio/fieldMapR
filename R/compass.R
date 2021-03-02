#' compass function
#'
#' function to (do something)
#'
#' @param x [value]
#' @param y [value]
#' @param angle [value]
#' @param xsize [value]. Default is 0.5
#' @param ysize [value]. Default is 1
#' @param txtSize [value]. Default is 1
#' @param pointyness [value]. Default is 3
#' @param col [value]. Default is "black"
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
compass <- function(x, y, angle, xsize = 0.5, ysize = 1, txtSize = 1, pointyness = 3, col = "black"){

	tri <- list(rbind(c(xsize, ysize), c(-xsize, ysize), c(0, pointyness * ysize)),
				rbind(c(ysize, xsize), c(ysize, -xsize), c(pointyness * ysize, 0)),
				rbind(c(xsize, -ysize), c(-xsize, -ysize), c(0, -pointyness * ysize)),
				rbind(c(-ysize, xsize), c(-ysize, -xsize), c(-pointyness * ysize, 0)))

	if(angle != 0){	
		triRot <- rotate(do.call(rbind, tri), angle)
		tri <- lapply(split(triRot, rep(1:length(tri), each = 3)), matrix, ncol = 2)
	}

	tri <- lapply(tri, function(z) sweep(z, 2, c(x, y), "+"))

	for(i in 1:length(tri)){
		if(i == 1){
			polygon(tri[[i]], col = col)
		} else {
			polygon(tri[[i]])
		}
	}

	text(x, y, labels = "N", cex = txtSize, srt = angle)
}
