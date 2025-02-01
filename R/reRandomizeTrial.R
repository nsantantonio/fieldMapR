#' reRandomizeTrial function
#'
#' function to re-randomize trials. Useful if the grower wants to plant multiple locations were blocks can be harvested for seed in some or all locations.
#'
#' @param ent [value]
#' @param rando previous randomization output from randomizeTrial()
#' @param test [value]
#' @param year [value]
#' @param loc [value]
#' @param grams [value]
#' @return [value] object of class fieldTrial
#' @details [fill in details here]
#' @examples # none
#' @export

reRandomizeTrial <- function(ent, rando, test = NULL, year = NULL, loc = NULL, grams = NULL){
	# head(ent)
	# head(rando)
	# dim(rando)
	randoInEnt <- rando[rando$Line %in% ent$Line,]
	# dim(randoInEnt)
	randoBlock <- list()
	for(i in unique(randoInEnt$Block)){
		blki <- randoInEnt[randoInEnt$Block == i,]
		newOrder <- sample(1:nrow(blki))
		blki <- blki[newOrder,]
		blki$Plot <- min(blki$Plot):{min(blki$Plot) + nrow(blki) - 1}
		randoBlock[[i]] <- blki
	}

	newRando <- do.call(rbind, randoBlock)
	if(!is.null(test)) newRando$Test <- test
	if(!is.null(year)) newRando$Year <- year
	if(!is.null(loc)) newRando$Location <- loc
	if(!is.null(grams)) newRando$grams <- grams
	newRando$Trial <- paste0(newRando$Test, "_", newRando$Year, "_", newRando$Location)
	newRando$plot_name <- paste0(newRando$Trial, "_", newRando$Plot)
	return(newRando)
}