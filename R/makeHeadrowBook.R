#' makeHeadrowTrays function
#'
#' function to make data.frame of headrows from a list of trays and checks.  
#'
#' @param trayName vector for the tray name
#' @param pop vector of population names
#' @param ped vector of pedigrees
#' @param checks vector of check varieties
#' @param nTrayPerPop number of trays per population, default is  3
#' @param trayL tray length, default is  80
#' @param checkSetSize number of checks per set, default is  4
#' @param frontBack checks at front and back of each populationo, default is  TRUE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
makeHeadrowBook <- function(trayName, pop, ped, checks, nTrayPerPop = 3, trayL = 80, checkSetSize = 4, frontBack = FALSE){
# trayName = srwHR$tray; pop = srwHRscotts$popNo; ped = srwHRscotts$simplePed; checks=checkLines$Line; nTrayPerPop = 3; trayL = 80; checkSetSize = 4; frontBack = FALSE
	if(length(checks) %% checkSetSize != 0) stop("length(checks) not a multiple of checkSetSize!")

	chkL <- split(checks, rep(1:checkSetSize, each = length(checks) / checkSetSize))
	# chkL[[5]] <- paste0("c", 1:4)
	nChkSets <- length(chkL)

	if(frontBack){
		N <- nTrayPerPop * trayL - checkSetSize * 2
		chkSetsPerPop <- 2
		# chkSetsPerPop <- 3
	} else {
		# stop("I can only do front back checks sets right now!")
		N <- nTrayPerPop * trayL - checkSetSize
		chkSetsPerPop <- 1
	}

	chkN <- 1:chkSetsPerPop
	cnt <- 1
	rowN <- 1
	hrL <- list()
	for(i in unique(pop)){
		whichPop <- which(pop == i)
		start <- min(whichPop)
		end <- max(whichPop)
		
		startChecks <- data.frame(Row = NA, Population = chkL[[chkN[1]]], Pedigree = "CHECK", tray = trayName[start])
		if(frontBack) {
			trays <- rep(trayName[start:end], times = c(c(trayL - checkSetSize), rep(trayL, diff(c(start, end)) - 1), c(trayL - checkSetSize)))
			hr <- data.frame(Row = NA, Population = rep(i, N), Pedigree = ped[start], tray = trays)
			endChecks <- data.frame(Row = NA, Population = chkL[[chkN[2]]], Pedigree = "CHECK", tray = trayName[end])
			hr <- rbind(startChecks, hr, endChecks)
		} else {
			trays <- rep(trayName[start:end], times = c(c(trayL - checkSetSize), rep(trayL, diff(c(start, end)))))
			hr <- data.frame(Row = NA, Population = rep(i, N), Pedigree = ped[start], tray = trays)
			hr <- rbind(startChecks, hr)
		}

		hr$Row <- rowN:{rowN + nrow(hr) - 1}
		rowN <- max(hr$Row) + 1

		hrL[[cnt]] <- hr
		chkN <- {tail(chkN, 1)+1}:{tail(chkN, 1)+chkSetsPerPop}
		chkOver <- chkN > nChkSets
		if(any(chkOver)){
			chkN[chkOver] <- 1:sum(chkOver)
		}
		chkN
		cnt <- cnt + 1
	}
	headrows <- do.call(rbind, hrL)
	headrows$select <- ""
	headrows$notes <- ""
	return(headrows)
}