#' combine function
#'
#' function to (do something)
#'
#' @param x1 [value]
#' @param x2 [value]
#' @param pile [value]. Default is FALSE
#' @return [value]
#' @details [fill in details here]
#' @examples none
#' @export
combine <- function(x1, x2, pile = FALSE) {
	if (!all(x1@borderPasses == x2@borderPasses)) warning("'borderPasses' dont match! First borderPasses will be kept. Unexpected behavior may result downstream")
	# if (!all(x1@trialName == x2@trialName)) warning("'trialName's dont match! First trialName will be kept. Unexpected behavior may result downstream")
	if (pile) {
		mat <- combineMat(x1, x2)
		stakes <- c(x1@needStake, x2@needStake)
	} else {
		add <- max(sapply(x2@needStake, "[[", 1))
		x1@needStake <- lapply(x1@needStake, function(x) {x[1] <- x[1] + add; x})
		stakes <- c(x1@needStake, x2@needStake)
		mat <- rbind(x2@matrix, x1@matrix)
	
	}
	fieldPlots(centers = c(x1@centers, x2@centers), 
			   corners = c(x1@corners, x2@corners), 
			   matrix = mat,
			   needStake = stakes,
			   borderPasses = x1@borderPasses,
			   trialName = x1@trialName)
	# for(i in names(x1)[sapply(x1, is.list)]) x[[i]] <- c(x1[[i]], x2[[i]])
}
