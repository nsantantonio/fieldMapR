#' T3ize function
#'
#' function to reformat line names using T3 rules
#'
#' @param x vector of line names to be reformatted 
#' @return formated line names
#' @details uses T3 rules to reformat line names
#' @examples 
#' T3ize(c("VA96W-403WS*HS", "VA19W-165*", "VA19W-165†", "Jagger", "MCIA Venus", "FFR 555"))
#' @export
T3ize <- function(x){
	rmSpace <- function(xx){
		len <- length(xx)
		if(length(xx) == 1) return(xx)		
		lpos <- nchar(xx[1])
		l <- substr(xx[1], lpos, lpos)
		s <- substr(xx[2], 1, 1)
		bothNum <- grepl("[0-9]", l) & grepl("[0-9]", s)
		bothChar <- grepl("[A-z]", l) & grepl("[A-z]", s)
		if(bothNum | bothChar) adapter <- "_" else adapter <- ""
		xx12 <- paste(xx[1], xx[2], sep = adapter)
		if(len > 2) xx <- c(xx12, xx[3:len]) else xx <- xx12
		rmSpace(xx)
	}
	x <- toupper(x)
	x <- gsub("\"|\'|\`|,", "_", x)
	
	# this might be a bad idea having the dagger match, I just dont know how to recognize that charactre
	endAst <- grep("\\*$|\\†$", x) 	
	x[endAst] <- gsub("\\*|\\†", "", x[endAst])
	midAst <- grep("\\*.+", x)
	x[midAst] <- gsub("\\*", "_", x[midAst])
	x <- gsub("__", "_", x)
	x <- trimws(x)
	xsplit <- strsplit(x, split = " ")
	xsplit <- lapply(xsplit, function(xx) xx[xx != ""])
	sapply(xsplit, rmSpace)
}