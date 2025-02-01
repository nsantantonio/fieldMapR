#' An S4 class to represent entry
#' @slot plotInfo  data.frame 
#' @export trialDesign
trialDesign <- setClass("trialDesign", slots = c(plotNo = "integer", plotName = "character", block = "integer", trialName = "character", Entry = 'integer', Line = "character", Pedigree = "character", plotInfo = "data.frame"))

#' @export 
length.trialDesign <- function(x){
	length(x@plotNo)
}

#' @export 
`[.trialDesign` <- function(td, i, ...){
	for(s in slotNames(td)){
		if(is.data.frame(slot(td,s))){
			slot(td,s) <- slot(td,s)[i,]
		} else {
			slot(td,s) <- slot(td,s)[i]
		}
	}
	td
}

#' @export 
`[[.trialDesign` <- function(td, i, ...){
	index <- which(td@block %in% i)
	for(s in slotNames(td)){
		if(is.data.frame(slot(td,s))){
			slot(td,s) <- slot(td,s)[index,]
		} else {
			slot(td,s) <- slot(td,s)[index]
		}
	}
	td
}

#' @export 
`as.data.frame.trialDesign` <- function(td){
	data.frame(Trial = td@trialName, plotName = td@plotName, Plot = td@plotNo, block = td@block, Entry = td@Entry, Line = td@Line, Pedigree = td@Pedigree)
}
