# assessment options

applyNoise.oem <- function (stk, idx, args, stockName, ...) {
  extraArgs <- list(...)
  
  # Get prev year
  ay <- args$ay
  year <- ac(ay-1)

  # Get noise parameters
  noiseParams.catch <- eval(parse(text = paste0(stockName, ".residual.params.catch")))
  noiseParams.index <- eval(parse(text = paste0(stockName, ".residual.params.index")))
  noiseParams.mean.stock <- eval(parse(text = paste0(stockName, ".residual.params.mean.stock")))
  noiseParams.vcratios.stock <- eval(parse(text = paste0(stockName, ".residual.params.vcratios.stock")))
  saParam <- eval(parse(text = paste0(stockName, ".assessment")))
  print(paste("Applying noise before", saParam))

  # Multiply catch and catch.n
  #print("Before")
  #print(catch.n(idx[[1]])[,year])
  #print(index(idx[[1]])[,year])
  #print(fbar(stk)[,year])
  #print(ssb(stk)[,year])

  # assessment option - SCAA
  if(saParam == "SCAA") {
    if(!is.null(noiseParams.catch)) {
      noise.catch <- apply(noiseParams.catch, 1, function(x) exp(rnorm(1, x[["mean"]], x[["sd"]])))
      catch.n(stk)[, year] <- catch.n(stk)[, year] * noise.catch

      # Recalculate catch
      catch(stk)[, year] <- sum(catch.n(stk)[, year] * catch.wt(stk)[, year])

      # Recalculate harvest
      harvest(stk)[, year] <- -log((stock.n(stk)[, year] - catch.n(stk)[, year])/stock.n(stk)[, year])
    }
    if(!is.null(noiseParams.index)) {
      noise.index <- apply(noiseParams.index, 1, function(x) exp(rnorm(1, x[["mean"]], x[["sd"]])))
      multiplyIndex <- function(singleIdx, nsOEM) {
        catch.n(singleIdx)[, year] <- catch.n(singleIdx)[, year] * nsOEM
        index(singleIdx)[, year] <- index(singleIdx)[, year] * nsOEM
        return(singleIdx)
      }

      idx <- lapply(idx, multiplyIndex, noise.index)
    }
  } else if(saParam == "truePlusNoise") {
    if(!is.null(noiseParams.mean.stock) && !is.null(noiseParams.vcratios.stock)) {
      # If using random different value for each age
      #noise.stock <- apply(noiseParams.stock, 1, function(x) exp(rnorm(1, x[["mean"]], x[["sd"]])))

      # Single value for all ages
      #noise.stock <- exp(rnorm(1, x[["mean"]], x[["sd"]]))

      # New noise calculation
      noise.stock <- mvrnorm(1, noiseParams.mean.stock[, 2], noiseParams.vcratios.stock[, -1])
      print(noise.stock)
      print(stock.n(stk)[, year])
      stock.n(stk)[, year] <- stock.n(stk)[, year] * noise.stock
      print(stock.n(stk)[, year])
  
      # Recalculate stock
      stock(stk)[, year] <- sum(stock.n(stk)[, year] * stock.wt(stk)[, year])
    }
  }

  #print("After")
  #print(catch.n(idx[[1]])[,year])
  #print(index(idx[[1]])[,year])
  #print(fbar(stk)[,year])
  #print(ssb(stk)[,year])

  return (list(stk = stk, idx = idx))
}

# assessment option - true plus noise
truePlusNoise.sa <- function (stk, idx, ...) {
    args <- list(...)
    tracking <- args$tracking 
    tracking["conv.est", ac(range(stk)["maxyear"] + 1)] <- 0
    list(stk = stk, tracking = tracking)
}

# assessment option - SAM
sam.sa <- function(stk, idx, ...) {
    args <- list(...)
    stk.ctrl <- FLSAM.control(stk, idx)
    fit <- FLSAM(stk, idx, stk.ctrl, return.fit = TRUE)
    stk.sam <- SAM2FLR(fit, stk.ctrl)
    stk     <- stk + stk.sam
    tracking <- args$tracking

    # TODO: Check below
    if(attr(fit$opt$objective, "logarithm"))
        tracking["conv.est", ac(range(stk)["maxyear"] + 1)] <- -log(as.numeric(fit$opt$objective))
    else
        tracking["conv.est", ac(range(stk)["maxyear"] + 1)] <- as.numeric(fit$opt$objective)

    list(stk = stk, tracking = tracking)
}
