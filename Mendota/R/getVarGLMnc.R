#getVarGLMnc.R (function)
#March 4, 2014
#Craig A. Snortheim

#Arguments:
#GLMnc [character]; ncdf4 object (returned by nc_open(GLMncFile))
#var [character]; variable name in GLMnc file
#lyrDz [numeric]; depth increment for "sampling"
#ref [character]; must be "bottom" or "surface"
#z.out

getVarGLMnc<- function(GLMnc, var, lyrDz= 0.25, ref= "bottom", z.out){
  
  #source the GLM.nc.R file (depthsampleGLM function not otherwise available)
  source("../../R/GLM.nc.R")
  
  #make sure the ref is either top or bottom
  if (ref != "bottom" & ref != "surface") {
    stop("reference input must be either \"surface\" or \"bottom\"")
  }
  NS <- ncvar_get(GLMnc, "NS")
  maxInd <- max(NS)
  elev <- ncvar_get(GLMnc, "z")
  vdata <- ncvar_get(GLMnc, var)
  if (length(dim(elev)) == 2) {
    elev <- elev[1:maxInd, ]
    vdata <- vdata[1:maxInd, ]
  }
  else {
    if (dim(elev) == 0) {
      stop("empty nc file")
    }
    else {
      elev <- elev[1:maxInd]
      vdata <- vdata[1:maxInd]
    }
  }
  rmvI <- which(vdata >= 1e+30 | elev >= 1e+30)
  elev[rmvI] <- NA
  vdata[rmvI] <- NA
  mxElv <- max(elev, na.rm = TRUE) + lyrDz
  mnElv <- min(elev, na.rm = TRUE) - lyrDz
  if (missing(z.out)) {
    if (ref == "surface") {
      elev.out <- seq(mnElv, mxElv, lyrDz)
      depth.out <- seq(0, mxElv - mnElv, lyrDz)
    }
    else {
      elev.out <- seq(mnElv, mxElv, lyrDz)
    }
    elev.out <- seq(mnElv, mxElv, lyrDz)
  }
  else {
    if (ref == "surface") {
      depth.out <- z.out
      elev.out <- seq(mnElv, mxElv, lyrDz)
    }
    else {
      elev.out <- mnElv + z.out
    }
  }
  time <- getTimeGLMnc(GLMnc)
  numStep <- length(time)
  numDep <- length(elev.out)
  vOut <- matrix(nrow = numStep, ncol = numDep)
  if (is.null(ncol(vdata))) {
    for (tme in 1:numStep) {
      x <- elev[tme]
      y <- vdata[tme]
      if (!is.na(y)) {
        ap <- approx(c(mnElv, x), c(y[1], y), xout = elev.out)
        vOut[tme, 1:length(ap$y)] <- ap$y
      }
    }
  }
  else {
    for (tme in 1:numStep) {
      cleanI <- which(!is.na(vdata[, tme]))
      x <- elev[cleanI, tme]
      y <- vdata[cleanI, tme]
      if (length(y) > 0) {
        ap <- approx(c(mnElv, x), c(y[1], y), xout = elev.out)
        vOut[tme, 1:length(ap$y)] <- ap$y
      }
    }
  }
  GLM <- data.frame(time)
  GLM <- cbind(GLM, vOut)
  frameNms <- letters[seq(from = 1, to = numDep)]
  frameNms[1] <- "DateTime"
  for (z in 1:numDep) {
    frameNms[z + 1] <- paste(c("elv_", as.character(elev.out[z])), 
                             collapse = "")
  }
  names(GLM) <- frameNms
  if (ref == "surface") {
    GLM <- depthsampleGLM(GLM, sampleDepths = depth.out)
  }
  return(GLM)
}
