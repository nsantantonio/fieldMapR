#' makeBreedbaseTrial function to create breedbase multi trial upload files 
#'
#' function to create multi trial upload files for breedbase
#'
#' @param trial object of class trialDesign
#' @param bags data.frame from a bag file with plot, range and pass information
#' @param breeding_program character string of length 1, with breeding program name
#' @param trial_type character string of length 1, with trial type (refer to breedbase for options)
#' @param design_type character string of length 1, with design type (refer to breedbase for options)
#' @param plantingDate optional, of the form 2020-10-23
#' @param harvestDate optional, of the form 2021-6-12
#' @param plot_width optional, numeric value for plot width
#' @param plot_length optional, numeric value for plot length
#' @param checks optional, character vector of check names
#' @param field_size numeric, field size in hectares 
#' @return data.frame formated breedbase multi trial upload file
#' @details uses T3 rules to reformat line names
#' @examples # none.
#' @export
makeBreedbaseTrial <- function(trial, bags = NULL, breeding_program = "Virginia Tech", trial_type = "phenotyping_trial", design_type = "RCBD", plantingDate = NULL, harvestDate = NULL, plot_width = NULL, plot_length = NULL, checks = NULL, field_size = NULL){
	# bags = allBags; program = NULL; trial_type = "phenotyping_trial"; design_type = "RCBD"; plantingDate = NULL; harvestDate = NULL; plot_width = NULL; plot_length = NULL; checks = NULL; field_size = NULL
	if(!is.null(bags)){
		bags$plotName <- paste0(bags$Trial, "_", bags$plot)
		bagtr <- bags[bags$plotName %in% trial@plotName,]
		if(nrow(bagtr) == 0){
			pass <- ""
			range <- ""
		} else {
			bagtr <- bagtr[order(as.numeric(bagtr$plot)),]
			if(!all(trial@plotName %in% bagtr$plotName)) {
				warning(paste0("Bags dont match trial plotNames for ", unique(trial@trialName), "!"))
				print(list(trial = trial@plotName, bags = bagtr$plotName))
				if(!all(trial@plotName[trial@plotName %in% bagtr$plotName] == bagtr$plotName)) stop(paste0("Bags not sortred correctly for ", unique(trial@trialName), ""))
				if(nrow(bagtr) > 0) trial <- trial[which(trial@plotName %in% bagtr$plotName)]
			} else {
				if(!all(bagtr$plotName == trial@plotName)) stop(paste0("Bags not sortred correctly for ", unique(trial@trialName), ""))
			}
			pass <- bagtr$pass
			range <- bagtr$range
		}
	} else {
		pass <- ""
		range <- ""
	}

	if(length(pass) == 0 | length(range) == 0){
		pass <- ""
		range <- ""
	}

	# if(is.null(program)) program <- "Virginia Tech"
	if(is.null(breeding_program)) breeding_program <- ""
	if(!is.null(trial@plotInfo$grams)) gramsPerPlot <- trial@plotInfo$grams else gramsPerPlot <- 75
	if(is.null(plantingDate)) plantingDate <- ""
	if(is.null(harvestDate)) harvestDate <- ""

	if(is.null(plot_width)) plot_width <- 1.524
	if(is.null(plot_length)) plot_length <- 2.7432

	if(!is.null(checks)){
		isCheck <- as.integer(trial@Line %in% checks)
	} else {
		isCheck <- ""
	}

	if(is.null(field_size)) field_size <- ""

	if(is.null(trial@plotInfo$src_num)) seedsource <- "" else seedsource <- trial@plotInfo$src_num

	classTrial <- class(trial)
	if(classTrial %in% "trialDesign"){
		trialName <- unique(trial@trialName)
		test <- gsub("_[^_]+$", "", trialName)
		year <- gsub(".*_", "", test)
		loc <- gsub(".*_", "", trialName)
		nursery <- gsub("_.*", "", test)

		if(loc == "BLAVA"){
	      location <- "Blacksburg, VA" 
	    } else if(loc == "WARVA") {
	        location <- "Warsaw, VA" 
	    } else if(loc == "PNTVA") {
	        location <- "Painter, VA"
	    } else if(loc == "BLSVA") {
	      location <- "Blackstone, VA"
	    } else if(loc == "HOLVA") {
	      location <- "Holland, VA"
	    } else if(loc == "ORGVA") {
	      location <- "Orange, VA"
	    } else if(loc == "SHVVA") {
	      location <- "Shenandoah Valley, VA"
	    } else if(loc == "NKTVA") {
	        location <- "New Kent, VA"
	    } else if(loc == "SN") {
	        location <- "Scab, VA"
	        loc <- "SCBVA"
	    } else {
	    	location <- NA
	    }


		trialdf <- data.frame(trial_name = trial@trialName,  
							  breeding_program = breeding_program, 
							  location = location,
							  year = year,
							  design_type = design_type, 
							  description = paste0("VT small grains trial ", trialName), 
							  trial_type = trial_type, 
							  plot_width = plot_width, 
							  plot_length = plot_length, 
							  field_size = field_size, 
							  planting_date = plantingDate, 
							  harvest_date = harvestDate, 
							  plot_name = trial@plotName, 
							  accession_name = trial@Line, 
							  plot_number = trial@plotNo, 
							  block_number = trial@plotInfo$Block, 
							  is_a_control = isCheck, 
							  rep_number = trial@plotInfo$Block, 
							  range_number = "", 
							  row_number = range, 
							  col_number = pass, 
							  seedlot_name = seedsource, 
							  num_seed_per_plot = "", 
							  weight_gram_seed_per_plot = gramsPerPlot
							  )

	} else if(classTrial %in% "fieldPlots"){
		stop("I cant do this with fieldPlots yet! Use an object of class 'trialDesign'")
		# trialdf <- 
	} else if(classTrial %in% "fieldBlock"){
		# trialdf <- trial@long
		# trial@plotNo
		stop("I cant do this with fieldBlocks yet! Use an object of class 'trialDesign'")

	}



	# trialVars <- c("trial_name", "breeding_program", "location", "year", "design_type", "description", "plot_name", "accession_name", "plot_number", "block_number")

	trialVars <- c("trial_name", 
	"breeding_program", 
	"location", 
	"year", 
	"design_type", 
	"description", 
	"trial_type", 
	"plot_width", 
	"plot_length", 
	"field_size", 
	"planting_date", 
	"harvest_date", 
	"plot_name", 
	"accession_name", 
	"plot_number", 
	"block_number", 
	"is_a_control", 
	"rep_number", 
	"range_number", 
	"row_number", 
	"col_number", 
	"seedlot_name", 
	"num_seed_per_plot", 
	"weight_gram_seed_per_plot")

	return(trialdf[trialVars])
}

# writeBreedbaseTrial <- function(breedbaseTrial, dir){
# 	WriteXLS(breedbaseTrial, paste0(dir, unique(breedbaseTrial$trial_name)), row.names = FALSE)
# 	# write.csv(breedbaseTrial, paste0(dir, unique(breedbaseTrial$trial_name)), row.names = FALSE)
# }
