#' An S4 class to represent field blocks, made up of field plots
#' @slot trial  matrix of trial
#' @slot plotNo  matrix of plotNo
#' @slot long  data.frame of long
#' @slot needStake  list of needStake
#' @slot borderPasses  numeric of borderPasses
#' @slot blockName  character of blockName
#' @export fieldBlock
fieldBlock <- setClass("fieldBlock", slots = c(Trial = "matrix", Line = "matrix", Entry = "matrix", plotName = "matrix", fill = "matrix", plotNo = "matrix", long = "data.frame", needStake = "list", borderPasses = "numeric", blockName = "character"))

# #' @export 
# length.fieldBlock <- function(fb){
# 	sum(!is.na(fb@plotNo))
# }


#' @export
length.fieldBlock <- function(x){
	sum(!is.na(x@plotNo))
}

#' @export
dim.fieldBlock <- function(x){
	dim(x@plotNo)
}

#' @export
nrow.fieldBlock <- function(x){
	nrow(x@plotNo)
}

#' @export
ncol.fieldBlock <- function(x){
	ncol(x@plotNo)
}