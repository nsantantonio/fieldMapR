#' plotField function
#'
#' function to make fieldBoundary from matrix of points. 
#'
#' @param m n x 2 matrix of points on boundary of field. the first row will be assumed to be the reference point if no argument is provided to 'reference'
#' @return object of class field boundary
#' @details [fill in details here]
#' @examples none
#' @export
matrixToDist <- function(m, reference = NULL){
	# if(class(boundary) != "fieldBoundry"){
	if(is.null(reference)) reference <- m[1,]
	if(!all(m[1,] == m[nrow(m),])) {
		m <- rbind(m, m[1,])
	}
	angle <- NULL
	for(i in 2:nrow(m)){
		vec <- m[i,]
		ref <- m[i-1,]
		angle[i-1] <- radToDeg(atan2(vec[1] - ref[1], vec[2] - ref[2]))
	}
	distance <- sapply(2:nrow(m), function(i) dist(m[c(i-1, i),])[1])
	
	fieldBoundary(points = m, angle = angle, distance = distance, reference = reference)
}