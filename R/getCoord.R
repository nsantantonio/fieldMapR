#' getCoord function
#'
#' function to (do something)
#'
#' @param plots [value]
#' @param plotNo [value]
#' @param corner [value]. Default is "bottomright"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
getCoord <- function(plots, plotNo, corner = "bottomright"){
	pos <- c(bottomleft = 1, bottomright = 2, topright = 3, topleft = 4)
	i <- pos[corner]
	plots@corners[[plotNo]][i,]
}
