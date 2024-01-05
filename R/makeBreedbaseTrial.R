#' makeBreedbaseTrial function to create breedbase multi trial upload files 
#'
#' function to create multi trial upload files for breedbase
#'
#' @param trial object of class trialDesign
#' @param locLookUp optional list or data.frame, list of length 2 with the first element being a character vector with location codes in the plot_name, and the second element being the long location name in City, State (abrv.) form. E.g. list(c("BLAVA", "WARVA"), c("Blacksburg, VA", "Warsaw, VA")) 
#' @param bags optinal data.frame from a bag file with plot, range and pass information. This is only needed if using trialDesigns for upload to include pass and range (trialDesigns dont have pass range information)
#' @param breeding_program character string of length 1, with breeding program name
#' @param trial_type character string of length 1, with trial type (refer to breedbase for options)
#' @param design_type character string of length 1, with design type (refer to breedbase for options)
#' @param planting_date optional, of the form 2020-10-23
#' @param harvest_date optional, of the form 2021-6-12
#' @param plot_width optional, numeric value for plot width
#' @param plot_length optional, numeric value for plot length
#' @param weight_gram_seed_per_plot optional, numeric value for grams of seed per plot
#' @param num_seed_per_plot optional, numeric value for number of seeds per plot
#' @param checks optional, character vector of check names
#' @param field_size numeric, field size in hectares 
#' @param description character vector, length 1 to append to begining of trial name for breedbase description
#' @return data.frame formated breedbase multi trial upload file
#' @details uses T3 rules to reformat line names
#' @examples # none.
#' @export
makeBreedbaseTrial <- function(trial, locLookUp = NULL, bags = NULL, rmFill = TRUE, breeding_program = "Virginia Tech", trial_type = "phenotyping_trial", design_type = "RCBD", planting_date = NULL, harvest_date = NULL, plot_width = NULL, weight_gram_seed_per_plot = NULL, num_seed_per_plot = NULL, plot_length = NULL, checks = NULL, field_size = NULL, description = NULL){
	# trial = trials[[i]]; bags = NULL; breeding_program = NULL; trial_type = "phenotyping_trial"; design_type = "RCBD"; planting_date = NULL; harvest_date = NULL; plot_width = NULL; plot_length = NULL; checks = NULL; field_size = NULL; weight_gram_seed_per_plot = NULL; num_seed_per_plot = NULL; description = NULL; locLookUp = NULL
	classTrial <- class(trial)
	
	if(classTrial %in% "trialDesign"){
		# put the other stuff here
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
		trial_name = trial@trialName
		plot_name = trial@plotName
		accession_name = trial@Line
		plot_number = trial@plotNo
		block_number = trial@plotInfo$Block
		rep_number = trial@plotInfo$Block

		if(!is.null(trial@plotInfo$grams) & is.null(weight_gram_seed_per_plot)) weight_gram_seed_per_plot <- trial@plotInfo$grams else weight_gram_seed_per_plot <- ""
		if(!is.null(trial@plotInfo$seedPerPlot) & is.null(num_seed_per_plot)) num_seed_per_plot <- trial@plotInfo$seedPerPlot else num_seed_per_plot <- ""
		if(is.null(trial@plotInfo$source)) seedsource <- "" else seedsource <- trial@plotInfo$source

	} else if(classTrial %in% "fieldBlock") {
		pass <- c(trial@pass)
		range <- c(trial@range)
		trial_name = c(trial@Trial)
		plot_name = c(trial@plotName)
		accession_name = c(trial@Line)
		plot_number = c(trial@plotNo)
		block_number = c(trial@Rep)
		rep_number = c(trial@Rep) # Note, as of right now, there is no difference between Rep and Block. May need to fix this later. 
		
		if(!is.null(trial@long$grams) & is.null(weight_gram_seed_per_plot)) weight_gram_seed_per_plot <- trial@long$grams else weight_gram_seed_per_plot <- ""
		if(!is.null(trial@long$seedPerPlot) & is.null(num_seed_per_plot)) num_seed_per_plot <- trial@long$seedPerPlot else num_seed_per_plot <- ""
		seedsource <- "" # need to fix so that fieldBlock keeps source information
	} else if(classTrial %in% "fieldBlock"){
		stop("I cant do this with fieldBlocks yet! Use an object of class 'trialDesign' or 'fieldBlock'")

	} else {
		stop("trial must be of class 'trialDesign' or 'fieldBlock'") # should I also allow fieldPlots? probably
	}

	# if(is.null(program)) program <- "Virginia Tech"
	if(is.null(breeding_program)) breeding_program <- ""
	if(is.null(planting_date)) planting_date <- ""
	if(is.null(harvest_date)) harvest_date <- ""

	if(is.null(plot_width)) plot_width <- 1.524
	if(is.null(plot_length)) plot_length <- 2.7432

	if(is.null(field_size)) field_size <- ""
	if(is.null(description)) description <- paste0("VT small grains trial ", trial_name) else description <- paste0(description, trial_name)

	if(!is.null(checks)){
		isCheck <- as.integer(accession_name %in% checks)
	} else {
		isCheck <- ""
	}


	test <- gsub("_[^_]+$", "", trial_name)
	year <- gsub(".*_", "", test)
	loc <- gsub(".*_", "", trial_name)
	nursery <- gsub("_.*", "", test)

	location <- loc
	
	# if(is.null(locLookUp)){
	# 	locLookUp <- list(c("BLAVA", "WARVA", "PNTVA", "BLSVA", "HOLVA", "ORGVA", "SHVVA", "NKTVA", "SN"),
	# 					  c("Blacksburg, VA", "Warsaw, VA", "Painter, VA", "Blackstone, VA", "Holland, VA", "Orange, VA", "Shenandoah Valley, VA", "New Kent, VA", "Scab, VA"))
	# }
	if(!is.null(locLookUp)){
		for(i in 1:length(locLookUp[[1]])){
			location[grepl(locLookUp[[1]][[i]], location, ignore.case = TRUE)] <- locLookUp[[2]][i]
		}
	}

	trialdf <- data.frame(trial_name = trial_name,  
						  breeding_program = breeding_program, 
						  location = location,
						  year = year,
						  design_type = design_type, 
						  description = description, 
						  trial_type = trial_type, 
						  plot_width = plot_width, 
						  plot_length = plot_length, 
						  field_size = field_size, 
						  planting_date = planting_date, 
						  harvest_date = harvest_date, 
						  plot_name = plot_name, 
						  accession_name = accession_name, 
						  plot_number = plot_number, 
						  block_number = block_number, 
						  is_a_control = isCheck, 
						  rep_number = rep_number, 
						  range_number = range, 
						  row_number = range, 
						  col_number = pass, 
						  seedlot_name = seedsource, 
						  num_seed_per_plot = num_seed_per_plot, # change this?
						  weight_gram_seed_per_plot = weight_gram_seed_per_plot
						  )

	if (rmFill) {
		trialdf <- trialdf[!trialdf$accession_name %in% "fill",]
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
