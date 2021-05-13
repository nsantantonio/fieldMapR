#' An S4 class to represent field blocks, made up of field plots
#' @slot trial  matrix of trial
#' @slot plotNo  matrix of plotNo
#' @slot long  data.frame of long
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot blockName  character of blockName
#' @export fieldBlock
fieldBlock <- setClass("fieldBlock", slots = c(trial = "matrix", plotNo = "matrix", long = "data.frame", needStake = "list", borderPasses = "numeric", blockName = "character"))
