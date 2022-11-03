#' tdToDf function
#'
#' function to turn an object of class 'trialDesign' into a data.frame
#'
#' @param td object of class 'trialDesign'
#' @param vars variables that should be included in the data.frame 
#' @return data.frame
#' @details [fill in details here]
#' @examples # none
#' @export
tdToDf <- function(td, vars = c("plotNo", "plotName", "block", "trialName", "Entry", "Line", "Pedigree")){
	l <- list()
	for(i in vars){
		l[[i]] <- slot(td, i)
	}
	as.data.frame(l)
}