#' getOutline function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
getOutline <- function(plots){
	corners <- plots@corners[!grepl("^[A-z]", names(plots@corners))]
	x <- do.call(rbind, corners)
	x[chull(x),]
}
