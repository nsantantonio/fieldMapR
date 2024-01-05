#' An S4 class to represent field plots
#' @name fieldPlots
#' @slot blocks list of blocks
#' @importFrom methods new slot slot<- slotNames
#' @export fieldPlots
fieldMap <- setClass("fieldMap", slots = c(blocks = c("list")))
# this has not yet been implemented. 