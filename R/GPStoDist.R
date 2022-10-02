#' GPStoDist function
#'
#' function to  make fieldBoundary from matrix of GPS points. Returns a flattened matrix in feet (default) or meters (when units argument set to 'm')
#'
#' @param boundary [value]
#' @param units [value]. Output units. Default is "ft" for ft, other can b
#' @param refPoint [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
GPStoDist <- function(boundary, units = "ft", refPoint = NULL){		
	if(is.null(refPoint)) refPoint <- apply(boundary, 2, min)
	reference <- refPoint
	boundriesRef <- rbind(refPoint, boundary, boundary[1,])
	ftPoints <- list()
	angles <- NULL
	distances <- NULL
	for(i in 2:nrow(boundriesRef)){

		refPoint <- if(i == 2) refPoint == c(0, 0) else ftPoints[[i-2]]
		nextPoint <- transLatLon(lat1 = boundriesRef[i-1, "lat"], 
									   lon1 = boundriesRef[i-1, "lon"], 
									   lat2 = boundriesRef[i, "lat"], 
									   lon2 = boundriesRef[i, "lon"], 
									   units = units, ref = refPoint)
		ftPoints[[i-1]] <- nextPoint$point
		angles[i-1] <- nextPoint$angle
		distances[i-1] <- nextPoint$distance
	}

	return(fieldBoundary(points = do.call(rbind, ftPoints), angle = angles, distance = distances, reference = reference))
}
