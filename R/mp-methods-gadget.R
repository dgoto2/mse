flsval <- list(object="stk", test="!is(object, \"FLS\")", msg="\"stk must be of class FLStock\"")

flival <- list(object="idx", test= "!is(object, \"FLIndices\")", msg="\"idx must be of class FLIndices\"")

flpval <- list(object="hcrpars", test= "!is(object, \"FLPar\")", msg="\"hcrpars must be of class FLPar\"")

flfval <- list(object="ctrl", test= "!is(object, \"fwdControl\")", msg="\"ctrl must be of class fwdControl\"")

flqval <- list(object="flq", test= "!is(object, \"FLQuant\")", msg="\"flq must be of class FLQuant\"")


mp.gadget <- function(input_multi){
	#============================================================
	# Get general parameters (from the first stock)
	#sr.om <- sr(input_multi[[stkName]]$opModel)
	#sr.om.res <- residuals(sr.om)
	#sr.om.res.mult <- sr.om@logerror
	it <- input_multi[[1]]$mpPars$it # Number of iterations
	fy <- input_multi[[1]]$mpPars$fy # final year
	y0 <- input_multi[[1]]$mpPars$y0 # initial data year
	#dy <- input_multi[[1]]$mpPars$dy # final data year
	iy <- input_multi[[1]]$mpPars$iy # initial year of projection (also intermediate)
	nsqy <- input_multi[[1]]$mpPars$nsqy # number of years to compute status quo metrics
	#ny <- fy - iy + 1 # number of years to project from intial year
	vy <- ac(iy:fy) # vector of years to be projected

	# Tracking metrics
	metric <- c("F.est", "B.est", "conv.est", "metric.hcr", "metric.is", "metric.iem", "metric.fb","F.om", "B.om", "C.om")

	# set seed (get from the first stock)
	if (!is.null(input_multi[[1]]$mpPars$seed)) set.seed(input_multi[[1]]$mpPars$seed)

	# Placeholder for list (for variables that might be updated)
	stk.om <- list()
	idx.om <- list()
	tracking <- list()
  	obsModel <- list()

	# Save the stock assessment result too
	stk0.sa <- list()
	idx0.sa <- list()

	#============================================================
	# go fish
	for(i in vy[-length(vy)]){
		# Do multiple flstocks
		for(stkName in stockList){
			
			impModel <- input_multi[[stkName]]$impModel
			ctrl0 <- input_multi[[stkName]]$ctrl
			mpPars <- input_multi[[stkName]]$mpPars
	
			#
			if(is.null(obsModel[[stkName]]))
				obsModel[[stkName]] <- input_multi[[stkName]]$obsModel

			# prepare the StockOm (check whether this is the first iter, if not use at it is)
			if(is.null(stk.om[[stkName]]))
				stk.om[[stkName]] <- stock(input_multi[[stkName]]$opModel)

			# prepare the IndexOm (check whether this is the first iter, if not use at it is)
			if(is.null(idx.om[[stkName]]))
				idx.om[[stkName]] <- input_multi[[stkName]]$indices
	
			name(stk.om[[stkName]]) <- input_multi[[stkName]]$scenario

			# init tracking (check whether this is the first iter, if not use at it is)
			if (is.null(tracking[[stkName]])){
				if (is.null(input_multi[[stkName]]$tracking)) 
					tracking[[stkName]] <- FLQuant(NA, dimnames=list(metric=metric, year=c(iy-1,vy), iter=1:it))
				else 
					tracking[[stkName]] <- input_multi[[stkName]]$tracking
			}

			# GET historical
			tracking[[stkName]]["metric.is", ac(iy)] <- catch(stk.om[[stkName]])[,ac(iy)]

			gc()
			ay <- mpPars$ay <- an(i)
			cat(i, " > ")
			mpPars$vy0 <- 1:(ay-y0) # data years (positions vector) - one less than current year
			sqy <- mpPars$sqy <- ac((ay-1):(ay-nsqy)) # years for status quo computations
		
			tracking[[stkName]]["F.om", ac(ay-1)] <- fbar(stk.om[[stkName]])[,ac(ay-1)]
			tracking[[stkName]]["B.om", ac(ay-1)] <- ssb(stk.om[[stkName]])[,ac(ay-1)]
			tracking[[stkName]]["C.om", ac(ay-1)] <- catch(stk.om[[stkName]])[,ac(ay-1)]
		
			#==========================================================
			# OEM
			#----------------------------------------------------------
			# function o()
			ctrl.oem <- args(obsModel[[stkName]])
			ctrl.oem$method <- method(obsModel[[stkName]])
			ctrl.oem$deviances <- deviances(obsModel[[stkName]])
			ctrl.oem$observations <- observations(obsModel[[stkName]])
			ctrl.oem$stk <- stk.om[[stkName]]
			ctrl.oem$args <- mpPars
			ctrl.oem$tracking <- tracking[[stkName]]
			ctrl.oem$ioval <- list(iv=list(t1=flsval), ov=list(t1=flsval, t2=flival))

			o.out <- do.call("mpDispatch", ctrl.oem)
			stk0 <- o.out$stk
			idx0 <- o.out$idx

			observations(obsModel[[stkName]]) <- o.out$observations
			tracking[[stkName]] <- o.out$tracking

			# We don't use OEM for the time being, use whatever value from gadget
			stk0 <- stk.om[[stkName]][, mpPars$vy0]
			idx0 <- idx.om[[stkName]]

			# Apply noise
			noisy.out <- applyNoise.oem(stk0, idx0, args=mpPars, stockName=stkName)
			idx0 <- noisy.out$idx
			stk0 <- noisy.out$stk
			
			#==========================================================
			# MP
			#----------------------------------------------------------
			# Estimator of stock statistics
			# function f()
			if (!is.null(ctrl0$est)){
				ctrl.sa <- args(ctrl0$est)
				ctrl.sa$method <- method(ctrl0$est)
				ctrl.sa$stk <- stk0
				ctrl.sa$idx <- idx0
				ctrl.sa$args <- mpPars
				ctrl.sa$tracking <- tracking[[stkName]]
				ctrl.sa$ioval <- list(iv=list(t1=flsval, t2=flival), ov=list(t1=flsval))
				out.assess <- do.call("mpDispatch", ctrl.sa)
				stk0 <- out.assess$stk
				tracking[[stkName]] <- out.assess$tracking
			}
			tracking[[stkName]]["F.est",ac(ay)] <- fbar(stk0)[,ac(ay-1)]
			tracking[[stkName]]["B.est",ac(ay)] <- ssb(stk0)[,ac(ay-1)]
	
			# Save SA results (note that only the first FLIndex is saved)
			if(is.null(stk0.sa[[stkName]])){
				tmpFLQ <- FLQuant(stock.n(stk0), dimnames=list(year=y0:fy))
				stk0.sa[[stkName]] <- FLStock(tmpFLQ)
				idx0.sa[[stkName]] <- FLIndex(tmpFLQ)
			}
			stk0.sa[[stkName]][,ac(ay-1)] <- stk0[,ac(ay-1)]
			idx0.sa[[stkName]][,ac(ay-1)] <- idx0[[1]][,ac(ay-1)]

			#----------------------------------------------------------
			# HCR parametrization
			# function x()
			if (!is.null(ctrl0$phcr)){
				ctrl.phcr <- args(ctrl0$phcr)
				ctrl.phcr$method <- method(ctrl0$phcr)
				ctrl.phcr$stk <- stk0
				ctrl.phcr$args <- mpPars
				ctrl.phcr$tracking <- tracking[[stkName]]
				if(exists("hcrpars")) ctrl.phcr$hcrpars <- hcrpars
				ctrl.phcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flpval))
				out <- do.call("mpDispatch", ctrl.phcr)
				hcrpars <- out$hcrpars
				tracking[[stkName]] <- out$tracking
			}

			#----------------------------------------------------------
			# HCR
			# function h()
			if (!is.null(ctrl0$hcr)){
				ctrl.hcr <- args(ctrl0$hcr)
				ctrl.hcr$method <- method(ctrl0$hcr)
				ctrl.hcr$stk <- stk0
				ctrl.hcr$args <- mpPars
				ctrl.hcr$tracking <- tracking[[stkName]]
				if(exists("hcrpars")) ctrl.hcr$hcrpars <- hcrpars
				ctrl.hcr$ioval <- list(iv=list(t1=flsval), ov=list(t1=flfval))
				out <- do.call("mpDispatch", ctrl.hcr)
				ctrl <- out$ctrl
				tracking[[stkName]] <- out$tracking
			} else {
				ctrl <- getCtrl(yearMeans(fbar(stk0)[,sqy]), "f", ay+1, it)
			}
			tracking[[stkName]]["metric.hcr", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]

			#----------------------------------------------------------
			# Implementation system
			# function k()
			if (!is.null(ctrl0$isys)){
				ctrl.is <- args(ctrl0$isys)
				ctrl.is$method <- method(ctrl0$isys)
				ctrl.is$ctrl <- ctrl
				ctrl.is$stk <- stk0
				ctrl.is$args <- mpPars
				ctrl.is$tracking <- tracking[[stkName]]
				ctrl.is$ioval <- list(iv=list(t1=flsval, t2=flfval), ov=list(t1=flfval))
				out <- do.call("mpDispatch", ctrl.is)
				ctrl <- out$ctrl
				tracking[[stkName]] <- out$tracking
				tracking[[stkName]]["metric.is", ac(ay)] <- ctrl@trgtArray[ac(ay+1),"val",]
			} else {
				tracking[[stkName]]["metric.is", ac(ay)] <- tracking[[stkName]]["metric.hcr", ac(ay+1)]
			}

			#----------------------------------------------------------
			# Technical measures
			# function w()
			if (!is.null(ctrl0$tm)){
				ctrl.tm <- args(ctrl0$tm)
				ctrl.tm$method <- method(ctrl0$tm)
				ctrl.tm$stk <- stk0
				ctrl.tm$args <- mpPars
				ctrl.tm$tracking <- tracking[[stkName]]
				ctrl.tm$ioval <- list(iv=list(t1=flsval), ov=list(t1=flqval))
				out <- do.call("mpDispatch", ctrl.tm)
				attr(ctrl, "snew") <- out$flq
				tracking[[stkName]] <- out$tracking
			}

			#==========================================================
			# IEM
			#----------------------------------------------------------
			if(!is.null(impModel)){
				ctrl.iem <- args(impModel)
				ctrl.iem$method <- method(impModel)
				ctrl.iem$ctrl <- ctrl
				ctrl.iem$args <- mpPars
				ctrl.iem$tracking <- tracking[[stkName]]
				ctrl.iem$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
				out <- do.call("mpDispatch", ctrl.iem)
				ctrl <- out$ctrl
				tracking[[stkName]] <- out$tracking
			}
			tracking[[stkName]]["metric.iem",ac(ay)] <- ctrl@trgtArray[,"val",]

			#==========================================================
			# OM
			#----------------------------------------------------------
			# fleet dynamics/behaviour
			# function j()
			if (exists(fleetBehaviour(input_multi[[stkName]]$opModel))){
				ctrl.fb <- args(fleetBehaviour(input_multi[[stkName]]$opModel))
				ctrl.fb$method <- method(fleetBehaviour(input_multi[[stkName]]$opModel))
				ctrl.fb$tracking <- tracking[[stkName]]
				ctrl.fb$ctrl <- ctrl
				ctrl.fb$args <- mpPars
				ctrl.fb$ioval <- list(iv=list(t1=flfval), ov=list(t1=flfval))
				out <- do.call("mpDispatch", ctrl.fb)
				ctrl <- out$ctrl
				tracking[[stkName]] <- out$tracking
			}
			tracking[[stkName]]["metric.fb",ac(ay)] <- ctrl@trgtArray[,"val",]

			#----------------------------------------------------------
			# stock dynamics and OM projections
			# function g()
			if(!is.null(attr(ctrl, "snew"))) harvest(stk.om[[stkName]])[,ac(ay+1)] <- attr(ctrl, "snew")

			print(paste("For stock:", stkName, "TAC is:", ctrl@trgtArray[, , iter = 1][["val"]]))
			
			# Apply any modifications to Gadget
			pre.gadget(ctrl=ctrl, stkName=stkName, year=i)
		} 
		#End--- multiple stocks computation

		# Forward gadget for one year
		fwd.gadget(year = i)
		
		# Collect the stocks for the new year
		for(stkName in stockList){
			postGadgetOut <- post.gadget(stk.om[[stkName]], idx.om[[stkName]][[1]], ctrl=ctrl, stkName=stkName, year=i)
			stk.om[[stkName]] <- postGadgetOut$stk
			idx.om[[stkName]][[1]] <- postGadgetOut$idx
		}
	}
	cat("\n")

	#============================================================
	# Collate output
	out <- list()
	for(stkName in stockList){
		out[[stkName]] <- list()
		out[[stkName]][["index"]] <- idx.om[[stkName]]
		out[[stkName]][["sa.result"]] <- list(stk0=stk0.sa[[stkName]], idx0=idx0.sa[[stkName]])
		units(out[[stkName]][["sa.result"]]$stk0)$harvest <- "f"
		out[[stkName]][["mse"]] <- as(input_multi[[stkName]]$opModel, "FLmse")
		stock(out[[stkName]][["mse"]]) <- stk.om[[stkName]]
		tracking(out[[stkName]][["mse"]]) <- tracking[[stkName]]
		args(out[[stkName]][["mse"]]) <- input_multi[[1]]$mpPars
	}
	return(out)
}
