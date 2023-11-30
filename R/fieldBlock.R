#' An S4 class to represent field blocks, made up of field plots
#' @slot trial  matrix of trial
#' @slot plotNo  matrix of plotNo
#' @slot long  data.frame of long
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot blockName  character of blockName
#' @export fieldBlock
fieldBlock <- setClass("fieldBlock", slots = c(Trial = "matrix", Line = "matrix", Entry = "matrix", Rep = "matrix", plotName = "matrix", fill = "matrix", plotNo = "matrix", range = "matrix", pass = "matrix", long = "data.frame", needStake = "list", borderPasses = "numeric", blockName = "character"))

#' @export
length.fieldBlock <- function(x){
	sum(!is.na(x@plotNo))
}

#' @export
dim.fieldBlock <- function(x){
	dim(x@plotNo)
}

#' nrow.fieldBlock
#' @param x object of class fieldBlock
#' @details determine number of rows (ranges) in fieldBlock
#' @export
nrow.fieldBlock <- function(x){
	nrow(x@plotNo)
}

#' ncol.fieldBlock
#' @param x object of class fieldBlock
#' @details determine number of columns (passes) in fieldBlock
#' @export
ncol.fieldBlock <- function(x){
	ncol(x@plotNo)
}

#' @export
print.fieldBlock <- function(x, ...){
	print(x@plotName, ...)
}