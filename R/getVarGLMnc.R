# getVarGLMnc
#' Extracts a data frame of time-depth series data of a variable from GLM output ncdf object
#' 
#' @param GLMnc An object of class ncdf4 containing the GLM output information
#' @param var A character string matching a variable name in nc object
#' @param lyrDz A numeric value specifying depth sample interval in meters (default is 0.5)
#' @param ref One of "surface" or "bottom", indicating where 0 depth is referenced
#' @param z.out Optional numeric vector of discrete depths to extract
#' 
#' @usage
#' getVarGLMnc(GLMnc, var, lyrDz= 0.5, ref= "surface", z.out)
#' 
#' @export
#' @return A data frame with number of columns equal to number of discrete depths plus one (DateTime)
#' 
#' @author Craig A. Snortheim
#' 
#' @examples 
#' require(ncdf4)
#' nc<- nc_open("sample.nc")
#' temp<- getVarGLMnc(GLMnc= nc, var= "temp", ref= "surface", z.out= c(0,5,10,15,20))
#' str(temp)
#' 
#' @references
#'  \url{http://aed.see.uwa.edu.au/research/models/GLM/}
#'  \url{https://github.com/GLEON}
#'  
#' @seealso \code{\link{ncvar_get}}, 
#'    \code{\link{depthsampleGLM}},
#'    \code{\link{getTimeGLMnc}}
#' 
#' @keywords rGLM
#' @keywords GLM-FABM

getVarGLMnc<- function(GLMnc, var, lyrDz= 0.25, ref= "surface", z.out){
  
  #get variables names from nc object
  ncvars<- names(GLMnc$var)
  
  #make sure the ref is either top or bottom
  if (ref != "bottom" & ref != "surface") {
    stop("reference input must be either \"surface\" or \"bottom\"")
  }
  
  #make sure var is a character string that appears once in the nc file variables
  if(!(var %in% ncvars)){
    stop("var input does not match a unique varible name in nc object")
  }
  
  NS <- ncvar_get(GLMnc, "NS")
  maxInd <- max(NS)
  elev <- ncvar_get(GLMnc, "z")
  vdata <- ncvar_get(GLMnc, var) 
  if(length(dim(vdata))==2){
    if (length(dim(elev)) == 2) {
      elev <- elev[1:maxInd, ]
      vdata <- vdata[1:maxInd, ]
    }else {
      if (dim(elev) == 0) {
        stop("empty nc file")
      }else {
        elev <- elev[1:maxInd]
        vdata <- vdata[1:maxInd]
      }
    }
  } else if(length(dim(vdata))==1){
    
    time<- getTimeGLMnc(GLMnc)
    GLM<- data.frame(time, as.numeric(vdata))
    names(GLM)<- c("DateTime", var)
    return(GLM)
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
    }else {
      elev.out <- seq(mnElv, mxElv, lyrDz)
    }
    elev.out <- seq(mnElv, mxElv, lyrDz)
  }else {
    if (ref == "surface") {
      depth.out <- z.out
      elev.out <- seq(mnElv, mxElv, lyrDz)
    }else {
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
