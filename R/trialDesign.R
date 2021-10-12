#' An S4 class to represent entry
#' @slot plotInfo  data.frame 
#' @export trialDesign
trialDesign <- setClass("trialDesign", slots = c(plotNo = "integer", plotName = "character", trialName = "character", Entry = 'integer', Line = "character", Pedigree = "character", plotInfo = "data.frame"))
