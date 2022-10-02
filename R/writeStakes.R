#' writeStakes function
#'
#' function to (do something)
#'
#' @param dir [value]. Default is ""
#' @param addPass [value]. Default is 0
#' @param every [value]. Default is 5
#' @param tooClose [value]. Default is 2
#' @param blockName [value]. Default is NULL
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
writeStakes <- function(..., dir = "", addPass = 0, every = 5, tooClose = 2, blockName = NULL){
	lclass <- lapply(list(...), class)
	if(all(lclass %in%  c("fieldPlots", "fieldBlock"))) {
		l <- lapply(list(...), printStakes, addPass = addPass, every = every, tooClose = tooClose, blockName = blockName)
	} else if(all(lclass %in%  "fieldStakes")) {
		l <- list(...)
	} else {
	stop("Arguments must be of class 'fieldStakes'")
	}
	count <- 0
	for(i in l){
		if(i@block == "") {
			count <- count + 1
			i@block <- as.character(count)
		}
		write.csv(i@long, file = paste0(dir, i@block, "Stakes.csv"), row.names = FALSE)
	}
}
