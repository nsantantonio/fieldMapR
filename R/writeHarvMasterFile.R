#' writeHarvMasterFile function
#'
#' function to (do something)
#'
#' @param block [value]. object of class 'fieldBlock'
#' @param dir [value]. Default is ""
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export


writeHarvMasterFile <- function(block, dir, ...){
	harvM <- makeHarvMasterFile(block, ...)
	write.csv(harvM, file = paste0(dir, "HarvMaster_", block@blockName, ".csv"), row.names = FALSE, quote = FALSE)
}