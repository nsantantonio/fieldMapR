#' combine function
#'
#' function to (do something)
#'
#' @param x1 [value]
#' @param x2 [value]
#' @param pile [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples # none
#' @export
combine <- function(x1, x2, pile = FALSE) {
	if(!(all(is.na(x1@borderPasses)) | all(is.na(x2@borderPasses)))) { # thought this might be needed. Need to check that it doesnt break something. 
	# if(!(all(is.na(x1@borderPasses)) & all(is.na(x2@borderPasses)))) {
		if (!all(x1@borderPasses == x2@borderPasses)) warning("'borderPasses' dont match! First borderPasses will be kept. Unexpected behavior may result downstream")
	}
	# if (!all(x1@trialName == x2@trialName)) warning("'trialName's dont match! First trialName will be kept. Unexpected behavior may result downstream")
	if (pile) {
		mat <- combineMat(x1, x2)
		fillmat <- combineMat(x1, x2, sname = "fill")
		repmat <- combineMat(x1, x2, sname = "Rep")
		entmat <- combineMat(x1, x2, sname = "Entry")
		linemat <- combineMat(x1, x2, sname = "Line")
		pedmat <- combineMat(x1, x2, sname = "Pedigree")
		stakes <- c(x1@needStake, x2@needStake)
	} else {
		add <- max(sapply(x2@needStake, "[[", 1))
		x1@needStake <- lapply(x1@needStake, function(x) {x[1] <- x[1] + add; x})
		stakes <- c(x1@needStake, x2@needStake)
		mat <- rbind(x2@matrix, x1@matrix)
		fillmat <- rbind(x2@fill, x1@fill)
		repmat <- rbind(x2@fill, x1@Rep)
		entmat <- rbind(x2@fill, x1@Entry)
		linemat <- rbind(x2@fill, x1@Line)
		pedmat <- rbind(x2@fill, x1@Pedigree)
	}

	trialName <- c(x1@trialName, x2@trialName)
	trialName <- trialName[trialName != "fill"][1]
	fieldPlots(centers = c(x1@centers, x2@centers), 
			   corners = c(x1@corners, x2@corners), 
			   matrix = mat,
			   fill = fillmat,
			   needStake = stakes,
			   borderPasses = x1@borderPasses,
			   trialName = trialName,
			   Rep = repmat, 
			   Entry = entmat, 
			   Line = linemat,
			   Pedigree = pedmat)
	# for(i in names(x1)[sapply(x1, is.list)]) x[[i]] <- c(x1[[i]], x2[[i]])
}
