#' getCenter function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
getCenter <- function(plots){
	colMeans(do.call(rbind, plots@centers))
}
