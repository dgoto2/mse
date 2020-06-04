###############################################################################
## testing the FLR/mse-Gadget MSE framework (IMR-REDUS project) - with the simple haddock example model
## started: sep 28, 2019; updated feb 12, 2020
###############################################################################

#==============================================================================
# libraries and auxiliary functions
#==============================================================================
library(mse)
library(dplyr)
library(FLa4a)
library(FLash)
library(FLAssess)
library(ggplotFL)
library(FLBRP)
library(FLCore)
library(MASS)
library(FLSAM)
library(filelock)

## Install latest gadgetr
#remotes::install_github("REDUS-IMR/gadget", ref="gadgetr")
## Install latest MSE from a4a
#remotes::install_github("flr/mse")
## Performance measurement
#library(profvis)

runOneTimeline <- function(iterSim, saveRaw) {
	
	## Set seed
	set.seed(0)

	## Setting directories
	codeDir <- paste0(homeDir,"codes")
	paramFileDir <- paste0(homeDir,"paramfiles")

	## Load gadget locally
	library(gadgetr)

	## Load helper functions
	source(paste0(codeDir,"/gadget-fls.R"), local=T)
	source(paste0(codeDir,"/overrides.R"), local=T)

	#==============================================================================
	# Load the stock and files
	#==============================================================================
	## Directory of the model
	setwd(paste0(homeDir, "/models/", modelName))

	## Load parameters
	paramsfile <- "refinputfile"
	gadget(c("-s", "-main", "main","-i", paramsfile))

	## Initialize simulation
	initSim()

	## Gadget various info
	stockList <- c("had")

	## stock information
	had.fleets <- c("comm", "survey", "future")
	had.stocks <- c("had")
	had.stocks.mature <- c("had")
	had.surveys <- c("survey")
	had.forecasts <- c("future")
	had.forecasts.tac.proportion <- c(0.232, 0.351, 0.298, 0.119)
	had.hcr.params <- list(method=ices.hcr, args=list(blim=100000, bsafe=200000, fmin=0.05, ftrg=0.15)) ## NEED TO CHANGE HCR
	
	## m2=NULL means we calculate m2 from gadget result, m2=0 means we use only residual mortality (m1)
	## StockStep: in which step we should observe the stock number
	had.params <- list(stockStep=2, minage=1, maxage=10, minfbar=2, maxfbar=8, startf=0.56, endf=0.65, areas=c(1), m1=c(0.2), m2=NULL) #TODO: gadget params
	
	## Recruitment parameters, if read csv (data frame) will apply the values accordingly, if a constant value, will apply the value as mux, if NULL 
	## leaving the recruitment params as it is
	#had.recruit.params <- read.csv(paste0(paramFileDir, "/muxfactors_cod.csv"))
	had.recruit.params <- 10.75871423
	#had.recruit.params <- NULL ## recruit params
	
	## assessment functions (truePlusNoise, SCAA, and SAM)
	## If truePlusNoise is chosen the noise using the residual params will be applied to stock only
	## If SCAA is chosen, the noise will be applied to both catch and index
	had.residual.params.catch <- read.csv(paste0(paramFileDir, "/had_resid_pars_catch.csv"))
	had.residual.params.index <- read.csv(paste0(paramFileDir, "/had_resid_pars_index.csv"))
	had.residual.params.stock <- read.csv(paste0(paramFileDir, "/had_resid_pars_stock.csv"))
	had.residual.params.mean.stock <- NULL
	had.residual.params.vcratios.stock <- NULL
	had.assessment <- "truePlusNoise" ## "truePlusNoise" or "SCAA" or "SAM"
	
	## If you don't want to apply error:
	#had.residual.params <- NULL
	had.noteating.forecast <- FALSE
	
	## Global simulation information
	firstYear <- 1978
	projYear <- 2000 
	finalYear <- 2020 

	## For gadget output in forecasts
	gadgetOut <-list()

  ## For performance measurements
  #test <- profvis({
	## Run until the start of projected year
	gadgetOut <- runUntil(projYear-1)
#})
#
#print(test)
#browser()

	## Preparing the MSE loop parameters for each stocks
	prepareStock  <- function(stockNameGl) {

		## Take a stock
		gadget.ret <- gadgetOut[[stockNameGl]]
		stk <- gadget.ret$stk
		idx <- FLIndices(a=gadget.ret$idx)

		#==============================================================================
		# Variables
		#==============================================================================
		it <- 1 # iterations
		fy <- finalYear #final year
		y0 <- range(stk)["minyear"] # initial data year
		dy <- range(stk)["maxyear"] # final data year
		iy <- projYear # initial year of projection (also intermediate)
		ny <- fy - iy + 1 # number of years to project from intial year
		nsqy <- 3 # number of years to compute status quo metrics
		vy <- ac(iy:fy) # vector of years to be projected
		management_lag <- 1 # For ICES HCR

		## Set up future assumptions - means of 5 years
		stk <- stf(stk, fy-dy, nsqy, nsqy)

		#==============================================================================
		# Fleet behaviour
		#==============================================================================
		fb <- mseCtrl(method=hyperstability.fb, args=list(beta=0.8))

		#==============================================================================
		# OM object
		#==============================================================================
		om <- FLom(stock=stk)#, fleetBehaviour=fb)
		#save(om, it, fy, y0, dy, iy, ny, nsqy, vy, fit, file="om.RData")

		###############################################################################
		# OEM settings
		###############################################################################

		#==============================================================================
		# prepare objects
		#==============================================================================
		idx <- FLIndices(a=gadget.ret$idx)
		stk <- stock(om)
		stk0 <- stk

		#==============================================================================
		# Estimate the indices catchability from the a4a fit (without simulation)
		#==============================================================================
		## Use all indices
		idcs <- FLIndices()
		for (i in 1:length(idx)){
			## this is a simplification as if index reflects 01 January abundances
			lst <- mcf(list(idx[[i]]@index, stock.n(stk0)))
			
			## log catchability of index
			idx.lq <- log(lst[[1]]/lst[[2]])
			
			## empty quant
			idx.qmu <- idx.qsig <- stock.n(iter(stk,1))
			
			## Every year has the same mean catchability
			idx.qmu[] <- yearMeans(idx.lq)
			idx.qsig[] <- sqrt(yearVars(idx.lq))
			idx.q <- FLQuant(NA, dimnames=dimnames(stock.n(stk)))
			
			## Build FLQ of index catchability based on lognormal distribution with mean and sd calculated above
			idx.q <- rlnorm(it, idx.qmu, idx.qsig)
			#idx.q[,ac(y0:iy)] <- idx.q[,ac(y0:iy)]
			idx_temp <- idx.q * stock.n(stk)
			
			## generate initial index
			idx_temp <- FLIndex(index=idx_temp, index.q=idx.q)
			range(idx_temp)[c("startf", "endf")] <- c(0, 0)
			idcs[[i]] <- idx_temp
		}
		names(idcs) <- names(idx)
		#idx <- FLIndices(a=idcs$a)

		#==============================================================================
		# Deviances for catch.n
		#==============================================================================
		#catch.dev <- log(catch.n(stk))
		#catch.dev <- catch.dev-iterMeans(catch.dev)
		#Sig <- apply(catch.dev[,ac(y0:dy),1,1,,drop=TRUE], 3, function(x) cov(t(x)))
		#Sig <- apply(Sig, 1, mean)
		#Sig <- matrix(Sig, ncol=dim(catch.dev)[1])
		#catch.dev[,ac(vy)][] <- t(mvrnorm(it * length(vy), rep(0, nrow(Sig)), Sig))
		#catch.dev <- exp(catch.dev)

		#==============================================================================
		# OEM object
		#==============================================================================
		idxDev <- lapply(idcs, index.q)
		names(idxDev) <- "index.q"
		stkDev <- FLQuant()
		dev <- list(idx=idxDev, stk=stkDev)
		obs <- list(idx=idcs[1], stk=stk)
		#oem <- FLoem(method=sampling.oem, args=list(oe="index"), observations=obs, deviances=dev)
		oem <- FLoem()
		#save(oem, file="oem.RData")

		###############################################################################
		## Implementation error
		###############################################################################
		#iem <- FLiem(method=noise.iem, args=list(fun="rlnorm", mean=0, sd=0.1, multiplicative=TRUE))
		iem <- FLiem()

		###############################################################################
		## Management procedure
		###############################################################################
		## general pars
		mpPars <- list(seed=1234, fy=fy, y0=y0, dy = dy, iy=iy, management_lag = management_lag, nsqy=nsqy, it=it)

		#==============================================================================
		## Scenarios
		#==============================================================================
		## Tell stocks to stop eating (if requested)
		if(eval(parse(text=paste0(stockNameGl, ".noteating.forecast")))){
			stockCat <- eval(parse(text=paste0(stockNameGl, ".stocks")))
			tmp <- lapply(stockCat, stopEating)
			print("Stocks stop eating now")
			print(tmp)
		}

		## Get HCR parameters
		hcrParams <- eval(parse(text=paste0(stockNameGl, ".hcr.params")))

		## Get SA parameter
		saParam <- eval(parse(text=paste0(stockNameGl, ".assessment")))
		if(saParam == "truePlusNoise") saMethod <- truePlusNoise.sa
		else if(saParam == "SCAA") saMethod <- sca.sa
		else if(saParam == "SAM") saMethod <- sam.sa

		## base with TAC
		ctrl <- list(hcr = mseCtrl(method=hcrParams[["method"]], args=hcrParams[["args"]]),
					isys = mseCtrl(method=tac.is),
					est = mseCtrl(method=saMethod))

		## Scenario name
		scenarioName <- paste0(stockNameGl, ".", "iter", iterSim)

		return(list(opModel=om, indices=idx, obsModel=oem, impModel=NULL, ctrl.mp=ctrl, mpPars=mpPars, scenario=scenarioName, tracking=NULL))
	}

	## load helpers
	source(paste0(codeDir,"/gadget-fwd.R"), local=T)
	source(paste0(codeDir,"/mp-methods-gadget.R"), local=T)

	inputPre <- lapply(stockList, prepareStock)
	names(inputPre) <- stockList
	res <- mp.gadget(inputPre)

	#return(list(mseResults=res,gadgetResults=gadgetOut))
	return(list(mseResults=res))
}

# Enable below to run directly from R shell
combIndex <- 1
iterIndex <- 1

# Global variables
homeDir <- paste0(getwd(),"/../")
modelName <- "had"
saveAllRawData <- FALSE

# Read effort combination
#print(paste("combination no.", combIndex, "iteration", iterIndex))
#fComb <- read.csv(paste0(homeDir, "paramfiles/effort_combination.csv"))

## Run with combination and iterIndex
resultFinal <- runOneTimeline(iterIndex, saveAllRawData)

## Name for the results
outFileName <- paste0(homeDir,"/results-combination", combIndex, ".rds")

## Use lock to prevent race condition when combining results
lck <- lock(paste0(outFileName, ".lock"))

## Check old results
if(file.exists(outFileName)) {
	# Load old results
	allResults <- readRDS(outFileName)
} else {
	# First result
	allResults <- list()
}

# Combine results
allResults[[iterIndex]] <- resultFinal

# See plot for HAD
stk.plot <- plot(FLStocks(stk.om = allResults[[iterIndex]]$mseResults$had$mse@stock, stk.mp = allResults[[iterIndex]]$mseResults$had$sa.result$stk0)) +
	theme(legend.position="top") + geom_vline(aes(xintercept=2000))

# Save combination info too
write.table(1, file=(paste0(outFileName,".info.txt")))

# Save it back
saveRDS(allResults, file=outFileName)

# Unlock the file
unlock(lck)
