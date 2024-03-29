#' getOutline function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @importFrom grDevices chull
#' @export
getOutline <- function(plots){
	# corners <- plots@corners[!grepl("^[A-z]", names(plots@corners))]
	corners <- plots@corners
	x <- do.call(rbind, corners)
	x[chull(x),]
}
