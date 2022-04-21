#' makePlots function
#' function to make fill plots. Wrapper for makePlots where all plots are fill.
#' @param n integer vector to determine number of fill plots
#' @param .... Arguments passes to makePlots
#' @return object of class fieldPLots
#' @details [fill in details here]
#' @examples none
#' @export
makeFill <- function(n, ...){
	l <- list(...)
	if("B" %in% names(l)){
		B <- l[["B"]]
	} else if ("B" %in% ls(envir = .GlobalEnv)) {
    	get("B", envir = .GlobalEnv)
  	} else {
    	B <- 1
  	}
	if(any(names(l) %in% "borderName")) bname <- l[["borderName"]] else bname <- "B"
	if(!any(names(l) %in% "trial")) l[["trial"]] <- "fill"
	fillPlots <- paste0(bname, {B}:{B + n - 1})
	B <<- B + length(fillPlots)
	def <- list(plotNo = fillPlots, borderPlotNo = B, updateB = FALSE)
	l <- c(def, l[!names(l) %in% def])
	fill <- do.call(makePlots, l)

	return(fill)
}

