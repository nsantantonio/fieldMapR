#' An S4 class to represent field blocks, made up of field plots
#' @slot points (n x 2) matrix of ordered field boundary points
#' @slot angle  numeric vector of n-1 of angles between points in degrees
#' @slot distance  distance between pairwise boundary points
#' @slot reference  nuermic vector of length 2, definingthe reference point
#' @slot borderPasses  numeric of borderPasses
#' @slot blockName  character of blockName
#' @export fieldBlock
fieldBoundary <- setClass("fieldBoundary", slots = c(points = "matrix", angle = "numeric", distance = "numeric", reference = "numeric"))

#' @export 
length.fieldBoundary <- function(x){
	nrow(x@points)
}

#' @export 
`[.fieldBoundary` <- function(fb, i, ...){
	for(s in slotNames(fb)){
		if(is.matrix(slot(fb,s))){
			slot(fb,s) <- slot(fb,s)[i,]
		} else {
			slot(fb,s) <- slot(fb,s)[i]
		}
	}
	fb
}