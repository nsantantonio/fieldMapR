#' flipInt function
#'
#' function to (do something)
#'
#' @param x [value]
#' @param maxx [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
flipInt <- function(x, maxx = NULL) {
	if(is.null(maxx)) maxx <- max(x)
	(x - maxx - 1) * -1
}
