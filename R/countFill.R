#' countFill function
#'
#' function to (do something)
#'
#' @param l [value]
#' @param fill [value]. Default is "fill"
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
countFill <- function(l, fill = "fill"){
	if(all(sapply(l, is.list))) l <- lapply(l, "[[", "long")
	fillcount <- NULL
	for(i in l){
		fillcount <- c(fillcount, sum(i$plotNo == fill))
	}
	sum(fillcount)
}
