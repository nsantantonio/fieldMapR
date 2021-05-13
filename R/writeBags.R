#' writeBags function
#'
#' function to (do something)
#'
#' @param dir [value]. Default is ""
#' @param addPass [value]. Default is 0
#' @param blockName [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
writeBags <- function(..., dir = "", addPass = 0, countBorder = TRUE, blockName = NULL){
	# if(!all(lapply(list(...), class) == "harvestBags")) stop("Arguments must be of class 'harvestBags'")
	lclass <- lapply(list(...), class)
	if(all(lclass %in%  c("fieldPlots", "fieldBlock"))) {
		l <- lapply(list(...), printBags, addPass = addPass, countBorder = countBorder, blockName = blockName)
	} else if(all(lclass %in%  "harvestBags")) {
		l <- list(...)
	} else {
		stop("Arguments must be of class 'harvestBags'")
	}
	count <- 0
	for(i in l){
		if(i@block == "") {
			count <- count + 1
			i@block <- as.character(count)
		}
		write.csv(i@long, file = paste0(dir, i@block, "Bags.csv"), row.names = FALSE)
	}
}
