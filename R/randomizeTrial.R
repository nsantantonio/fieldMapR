#' randomizeTrial function
#'
#' function to randomize trials
#'
#' @param ent [value]
#' @param test [value]
#' @param year [value]
#' @param loc [value]
#' @param reps [value]
#' @param grams [value]
#' @param randomize [value]. Default is 'RCBD'
#' @param entryGrams [value]. Default is NULL
#' @param otherCols [value]. Default is NULL
#' @return [value] object of class fieldTrial
#' @details [fill in details here]
#' @examples # none
#' @export

# need to fix, sometimes doesnt equally proportion blocks. I dont know why...

# ent = entry; test = test; year = year; loc = locs$Location; reps = reps; grams = locs$grams; randomize = design
# ent = entry; testName = "SRWPrelim"; year = 2022; loc = c("BLAVA", "WARVA", "PNTVA"); reps = 2; grams = 75
# reps <- c(3, 2, 1)
# reps <- rep(3, nrow(ent)); reps[ent$Notes == "Year2"] <- 2; reps[ent$Notes == "Year1"] <- 1
randomizeTrial <- function(ent, test, year, loc, reps, grams, randomize = 'RCBD', entryGrams = NULL, otherCols = NULL){
	# ent = entry; test = test; year = year; grams = locs$grams; loc = locs$Location; reps = reps;  entryGrams = entryGrams; randomize = design
	ent$Test <- test
	ent$Year <- year
	if(length(grams) == 1){
		grams <- rep(grams, length(loc))
	} 
	names(grams) <- loc
	
	if(!is.null(entryGrams)){
		gramsByEnt <- TRUE
	} else {
		gramsByEnt <- FALSE
	}
	locEntGrams  <- loc[is.na(grams)]
	
	# ent$grams <- grams
	# if(is.null(ent$src_type)) ent$src_type <- NA
	# if(is.null(ent$src_num)) ent$src_num <- NA
	if(is.null(ent$source)) ent$source <- NA


	nEnt <- nrow(ent)
	if(nEnt < 100) series <- 100 else if(nEnt < 1000) series <- 1000 else series <- 10000

	locReps <- list()	
	if(all(reps == 1)){
		for(i in loc){
			if(randomize %in% c("NO", "No", "no")){
				rEnt <- 1:nrow(ent)
			} else {
				rEnt <- sample(1:nrow(ent))
			}
			enti <- ent[entSample,]				
			enti$Plot <- 1:nrow(enti)
			enti$Block <- 1
			if(i %in% locEntGrams & gramsByEnt) enti$grams <- entryGrams[rEnt] else enti$grams <- grams[i]
			locReps[[i]] <- enti
		}
	} else if (randomize == "RCBD"){	
		if(length(reps) == length(loc) & all(loc %in% names(reps))){
			reps <- reps[loc]
		} else if(length(reps) == length(loc)){
			names(reps) <- loc
		} else if(length(reps) == 1) {
			reps <- rep(reps, length(loc))
			names(reps) <- loc
		} else {
			stop("Length of reps is not 1, nor length loc. Please specify number of reps per loc!")
		}
		for(i in loc){
			for(j in 1:reps[i]){
				rEnt <- sample(1:nEnt)
				entij <- ent[rEnt, ]
				entij$Plot <- {series * j + 1}:{series * j + nEnt}
				entij$Block <- j
				# entij$grams <- grams[i]
				if(i %in% locEntGrams & gramsByEnt) entij$grams <- entryGrams[rEnt] else entij$grams <- grams[i]
				locReps[[i]][[j]] <- entij
			}
			if(length(locReps[[i]]) > 1) locReps[[i]] <- do.call(rbind, locReps[[i]]) else locReps[[i]] <- locReps[[i]][[1]]
		}
	} else if (randomize == "p-rep" & length(reps) > loc){
		stop("This hasnt been implemented yet!")
		# a true p-rep design uses a tabu search across the grid to minimize PEV. really want to implement this. 
	} else if (randomize == "augmented"  & length(reps) > length(loc)){
		repClass <- class(reps)
		if(repClass %in% c("numeric", "integer")){
			reps <- matrix(reps, ncol = 1)
		} else if(repClass != "matrix"){
			stop("reps must be a numeric vector or matrix!")
		}

		if(nrow(reps) != nrow(ent)){
			stop("number of reps for each line must be specified for an augmented design!")
		}
		if(ncol(reps) == length(loc)){
			if(all(loc %in% colnames(reps))){
				reps <- reps[,loc]
			} else {
				colnames(reps) <- loc
			}
		} else if(ncol(reps) == 1) {
			reps <- do.call(cbind, lapply(loc, function(x) reps))
			colnames(reps) <- loc
		}

		warning("Augmented design is experimental!")
		# if(length(reps) != nrow(ent)){
		# 	stop("number of reps for each line must be specified for an augmented design!")
		# }

		repL <- list()
		for(i in loc){
			maxRep <- max(reps[,i])
			minRep <- min(reps[,i])
			repSize <- ceiling(sum(reps[,i]) / maxRep)
			
			if(repSize < 100) series <- 100 else if(repSize < 1000) series <- 1000 else series <- 10000

			repSlots <- rep(repSize, maxRep)
			repNos <- 1:maxRep 
			# repTab <- matrix(nEnt)
			for(j in 1:maxRep){
				ischeck <- reps[,i] == maxRep
				repL[[i]][[j]] <- which(ischeck)
				repSlots[j] <- repSlots[j] - sum(ischeck)
			}
			for (k in {maxRep-1}:minRep){
				nrl <- reps[,i] == k
				whichLine <- which(nrl)
				if(length(whichLine) > 1) whichLine <- sample(whichLine)
				for(l in whichLine){
					stillSlots <- repSlots != 0
					if(length(unique(repSlots[stillSlots]) == 1)){
						whichRep <- sample(1:length(repSlots[stillSlots]))[1:k]
					} else {
						whichRep <- repNos[order(repSlots[stillSlots], decreasing = TRUE)][1:k]
					}
					for(r in whichRep) {
						repL[[i]][[r]] <- c(repL[[i]][[r]], l)
					}
					repSlots[whichRep] <- repSlots[whichRep] - 1
				}
			}
			for(j in 1:maxRep){
				rEnt <- sample(repL[[i]][[j]])
				entij <- ent[rEnt, ]
				entij$Plot <- {series * j + 1}:{series * j + nrow(entij)}
				entij$Block <- j
				# entij$grams <- grams[i]
				# Not 100% sure this will work!
				if(i %in% locEntGrams & gramsByEnt) entij$grams <- entryGrams[rEnt] else entij$grams <- grams[i]
				locReps[[i]][[j]] <- entij
			}
			if(length(locReps[[i]]) > 1) locReps[[i]] <- do.call(rbind, locReps[[i]]) else locReps[[i]] <- locReps[[i]][[1]]			
		}
	}
	if(length(locReps) > 1) trialPlots <- do.call(rbind, locReps) else trialPlots <- locReps[[1]]
	trialPlots$Location <- rep(loc, times = sapply(locReps, nrow))
	trialPlots$Trial <- paste(trialPlots$Test, trialPlots$Year, trialPlots$Location, sep = "_")
	trialPlots$plot_name <- paste(trialPlots$Trial, trialPlots$Plot, sep = "_")
	# keepCols <- c("Test", "Year", "Location", "Trial", "Entry", "Line", "Block", "Plot", "plot_name", "grams", "Pedigree", "src_num", "src_type", otherCols)
	keepCols <- c("Test", "Year", "Location", "Trial", "Entry", "Line", "Block", "Plot", "plot_name", "grams", "Pedigree", "source", otherCols)
	
	if(any(!keepCols %in% names(trialPlots))){
		message("The following expected columns are not in the entry file:\n")
		print(keepCols[!keepCols %in% names(trialPlots)])
	}
	trialPlots[keepCols]
	# trialDesign(plotNo = trialPlots$Plot, plotName = "character", trialName = "character", Line = "character", Pedigree = "character", plotInfo = trialPlots[keepCols])
}
