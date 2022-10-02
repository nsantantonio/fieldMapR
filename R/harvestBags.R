#' An S4 class to represent information to be printed on harvest bags 
#'@slot plotNo  character of plotNo
#'@slot pass  numeric of pass
#'@slot range  numeric of range
#'@slot trial  character of trial
#'@slot block  character of block
#'@slot long  data.frame of long
#' @export harvestBags
harvestBags <- setClass("harvestBags", slots = c(Trial = "character", Line = "character", Entry = "integer", plotName = "character", plotNo = "character", pass = "numeric", range = "numeric", block = "character", long = "data.frame"))

