

# Gadget forward function
pre.gadget <- function(...)
{
  	# parse ... for ctrl
	args=list(...)

	stkName <- args$stkName
	year <- as.integer(args$year)

	forecastFleets <- eval(parse(text=paste0(stkName, ".forecasts")))

	# print current info (NOTE: This has to be local to process)
	#print(paste(stkName, year))
	#print(forecastFleets)
	#print(getEcosystemInfo())

	# Assuming we only take the 1st iter value
	TACs <- args$ctrl@trgtArray[, , iter = 1][["val"]]
	if(is.na(TACs)) TACs <- 0
	print(TACs)

	# Control the catch amount
	# Assuming equally distributed TAC (and only for COD)
	nFleet <- length(forecastFleets)
	TAC <- TACs/nFleet

	# Set TAC
	tacPortion <- eval(parse(text=paste0(stkName, ".forecasts.tac.proportion")))
	lapply(forecastFleets, updateAmount, year, 1, 1, tacPortion[[1]] * TAC)
	lapply(forecastFleets, updateAmount, year, 2, 1, tacPortion[[2]] * TAC)
	lapply(forecastFleets, updateAmount, year, 3, 1, tacPortion[[3]] * TAC)
	lapply(forecastFleets, updateAmount, year, 4, 1, tacPortion[[4]] * TAC)

	# Set recruitment
	recruitParams <- eval(parse(text=paste0(stkName, ".recruit.params")))
	matureStocks <- eval(parse(text=paste0(stkName, ".stocks.mature")))

	# Support NULL and single (fixed) recruitment parameter (mux)
	if(is.data.frame(recruitParams))
		recYearParam <- recruitParams[recruitParams$year==as.character(year), ncol(recruitParams)]
	else
		recYearParam <- recruitParams

	if(!is.null(recYearParam)){
		lapply(matureStocks, updateRecruitment, recYearParam, -1)
	}
}

# Gadget forward function
fwd.gadget <- function(...)
{
	# parse ... for ctrl
    	args=list(...)
	year <- as.character(args$year)
	print(paste("Now running gadget for ", year))
	gadgetYear <- as.character(getEcosystemInfo()$time[["currentYear"]])

	# Sanity check: Ensure year in MSE is the same with the gadget year
	if (year != gadgetYear){
		print(paste("Gadget year is", gadgetYear, ", while MSE year is", year))
		print("Gadget year is not the same as the MSE year. Stopping...")
		stop()
	}

	# If we don't want to save rawData, delete all the previous data and only save the last year
	if(!saveRaw){
		gadgetOut <<- list()
		gc()
	}

	# Run for one year, put it into global gadgetOut variable
	gadgetOut[[year]] <<- runYear()
}

# Post Gadget run (e.g., collect stats, etc.)
post.gadget <- function(object, objectIdx, ...)
{
	# parse ... for ctrl
	args=list(...)

	year <- as.character(args$year)

	print(paste("Now inserting gadget data for", year))

	# Update object (FLStock)
	updated <- updateFLStock(args$stkName, gadgetOut[[year]], year, object, objectIdx)
	object <- updated$stk
	objectIdx <- updated$idx

	return(list(stk=object, idx=objectIdx))
}
