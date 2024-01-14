#' plotField function
#'
#' function to make fieldBoundary from matrix of points. 
#'
#' @param m n x 2 matrix of points on boundary of field. the first row will be assumed to be the reference point if no argument is provided to 'reference'
#' @param reference reference point. If NULL, reference is assumed to be first row in m
#' @return object of class field boundary
#' @details [fill in details here]
#' @examples # none
#' @export
matrixToDist <- function(m, reference = NULL, close = TRUE){
	# if(class(boundary) != "fieldBoundry"){
	if(is.null(reference)) {
		reference <- m[1,]
	} else {
		if(length(reference) != 2) stop("'reference' needs to be length two!")
	}
	if(!all(m[1,] == m[nrow(m),]) & close) {
		m <- rbind(m, m[1,])
	}
	if(!all(reference == 0)){
		refDist <- dist(cbind(c(0,0), reference))
	} else {
		refDist <- 0
	}
	angle <- 0
	for(i in 2:nrow(m)){
		vec <- m[i,]
		ref <- m[i-1,]
		angle[i] <- radToDeg(atan2(vec[1] - ref[1], vec[2] - ref[2]))
	}
	distance <- c(refDist, sapply(2:nrow(m), function(i) dist(m[c(i-1, i),])[1]))
	
	fieldBoundary(points = m, angle = angle, distance = distance, reference = reference)
}