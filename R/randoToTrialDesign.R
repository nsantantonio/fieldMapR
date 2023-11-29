#' randoToTrialDesign function
#'
#' function to produce an object of class 'trialDesign' from the output of randomizeTrial
#'
#' @param rando data.frame from output of randomizeTrial
#' @param ignoreTrials character vector of trials to be ignored
#' @return [value] object of class trialDesign
#' @details [fill in details here]
#' @examples # none
#' @export
randoToTrialDesign <- function(rando, ignoreTrials = NULL, returnList = FALSE){
	trials <- list()
	trialsi <- unique(rando$Trial)
	if(any(names(rando) %in% "plotName") & !any(names(rando) %in% "plot_name")) {
		names(rando)[names(rando)  == "plotName"] <- "plot_name"
	}
	if(length(ignoreTrials)) {
		whichTrials <- !grepl(paste(ignoreTrials, collapse = "|"), trialsi)
		trialsi <- trialsi[whichTrials]
	}
	for(j in trialsi){
		rij <- rando[rando$Trial == j,]
		whichEmpty <- which(sapply(rij, function(x) all(is.na(x))))
		for(k in whichEmpty){
			class(rij[[k]]) <- "character"
		}
		trials[[j]] <- trialDesign(plotNo = rij$Plot, plotName = rij$plot_name, block = rij$Block, trialName = rij$Trial, Entry = rij$Entry, Line = rij$Line, Pedigree = rij$Pedigree, plotInfo = rij)
	}
	if(!returnList){
		if(length(trials) == 1){
			trials <- trials[[1]]
		}
	}
	return(trials)
}
