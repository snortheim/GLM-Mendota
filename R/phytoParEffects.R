#phytoParEffects.R (non-interactive R script)
#Craig A Snortheim
#March 27, 2014

#remove variables
rm(list= ls())

#SETTINGS#
###############################################################################
basewd<- "C:/Users/Craig/Documents/GitHub/GLM-Mendota/Mendota/sim/mendota_phyto_default"
simName<- "Mendota_phyto"
GLMncFile<- "output.nc"
exeName<- "glm_noplot.bat"
phytoFile<- "aed_phyto_pars.nml"
phytoList<- c("micro", "chloro", "diatom")
parsList<- c("Pmax", "vT")
parsInit<- c(0.5, 1.08, 0.5, 1.08, 1.25, 1.08) #order= parsList embedded in phytoList
sample.depth<- 2  #from surface
loop.counter<- TRUE

#manually set parameter ranges- list element [[i]] corresponds to 
#the parameter parsList[i]
valueList<- list() #do not edit
# valueList[[1]]<- seq(from= 0.1, to= 3, by= 0.1) #Pmax
# valueList[[2]]<- seq(from= 1.05, to= 1.15, by= 0.005) #vT

#TEST CASE (un/comment lines)
valueList[[1]]<- c(0.4, 0.8, 1.2, 1.6) #Pmax
valueList[[2]]<- c(1.04, 1.06, 1.08, 1.10) #vT

###############################################################################

#set working directory to the base WD
setwd(basewd)

#determine number of loops and files
numPhytos<- length(phytoList)
numPars<- length(parsList)
vc<- 0  #vc= value count
for(e in 1:numPars){
  vc<- vc + length(valueList[[e]])} #loop here to allow sum of uneven parameter range lengths
numLoops<- numPhytos * vc
numFiles<- numPhytos* numPars

#Check for errors in user entry:
#Consistency between initial parameter values, phytoplankton list and
#parameter list
if(numFiles != (length(parsInit))){
  error("Check parsInit variable- length must be equal to length of phytoList times
        length of parsList")
}
if(numPars != (length(valueList))){
  error("Check valueList variable- number of value sequences in valueList must
        equal the length of parsList")
}


#load all required libraries and functions
require(ncdf4)
require(rGLM)  ###inconsistencies with the reference argument in getX functions...
source("../../R/GLMnetCDF.R")
source("../../R/phytoNMLedit.R")
source("../../R/getVarGLMnc.R")


#set values in phyto parameter file to initial values specified in parsInit above
#order of values in parsInit is crucial here
count<- 0
for(py in 1:(length(phytoList))){
  for(pa in 1:(length(parsList))){
    count<- count + 1
    phytoNMLedit(filename= phytoFile, parName= parsList[pa], 
                 phytoName= phytoList[py], newVal= parsInit[count])
  }
}

#start inital parameter value counter for resetting pars after run
#used for index in parsInit vector
ipvCount<- 0

#create primary results folder (includes simName and dateTime)
sysTime<- as.character(Sys.time())
st1<- unlist(strsplit((unlist(strsplit(sysTime, " "))[1]), "-"))
st2<- unlist(strsplit((unlist(strsplit(sysTime, " "))[2]), ":"))
folderName<- paste(sep="",simName, st1[2], st1[3], st1[1], "_", st2[1], st2[2])
fpath<- paste("../../results/", folderName, sep= "")
dir.create(fpath)
  
#initialize loop counter (only printed if loop.counter== TRUE)
loop.count<- 0

#Begin triple nested loop (loop through phyto groups, parameters, & values)

for(y in 1:numPhytos){ #phyto loop
  #assign phytoplankon name
  phytoName<- phytoList[y]
  
  for(p in 1:numPars){ #parameter loop
    
    #assign parameter name
    parName<- parsList[p]
    
    #initial parameter value counter
    ipvCount<- ipvCount + 1
    
    #get number of values to preallocate vector length
    cnv<- length(valueList[[p]]) #cnv= current number of values for parameter
    
    #pre-allocate (and/or reset) matrices to store value value loop results
    phytoMeanMat<- matrix(nrow= cnv, ncol= (numPhytos+1))
    phytoPeakMat<- matrix(nrow= cnv, ncol= (numPhytos+1))
    phytoPeakTimeMat<- matrix(nrow= cnv, ncol= (numPhytos+1))
  
    for(v in 1:cnv){ #value loop
      
      #get new value from valueList
      newVal<- valueList[[p]][v]
      
      #set new value
      phytoNMLedit(filename= phytoFile, parName= parName, phytoName= phytoName, 
                   newVal= newVal, output= FALSE)
      
      #run GLM simulation
      system(exeName, intern= TRUE, show.output.on.console= FALSE, invisible= TRUE)

      #extract nc object from NetCDF file (not data)
      nc<- nc_open("output.nc")
    
      #get time information from nc file
      time<- getTimeGLMnc(nc)
      
      #find length of ts and preallocate phyto matrix
      ts<- length(time)
      phytoConc<- matrix(nrow= ts, ncol= numPhytos)
      
      #create var names for reading phyto data
      ncVarNames<- character(numPhytos)
      for(k in 1:numPhytos){
        ncVarNames[k]<- paste(sep= "", "aed_phytoplankton_", phytoList[k])
      }
      
      #set column names to nc variable names
      colnames(phytoConc)<- ncVarNames
      
      #extract data from NetCDF file
      for(kk in ncVarNames){
        phytoConc[,kk]<- getVarGLMnc(GLMnc= nc, var= kk, lyrDz= 0.5, 
                                        ref= "surface", z.out= sample.depth)[,2]
      }
    
      #bind an aed_phytoplankton_total column to the end of phytoBiomass matrix
      aed_phytoplankton_total<- rowSums(phytoConc)
      phytoConc<- cbind(phytoConc, aed_phytoplankton_total)
      
      #!#NOTE: indices of the time vector [class= POSIXct, length= number of 
      #time steps] correspond to the row indices of the phytoBiomass matrix
      #[class= matrix, col= numPhytos + 1, rows= time steps]
      #use this property to extract summer phyto data only (June thru August)
      
      #get summer time series indices
      sumIndx<- which(months(time) == "June" | months(time)== "July" | 
                        months(time)== "August")
      
      #find dimensions of matrix subset for mean() functions
      mm<- length(sumIndx)
      nn<- numPhytos + 1
      
      #find summer mean concentrations
      #!#NOTE: na.rm argument is set to TRUE- NA values will be ignored
      sumMeanConc<- base::.colMeans(X= phytoConc[sumIndx,], m= mm, n= nn, na.rm= TRUE)
      names(sumMeanConc)<- colnames(phytoConc)
      
      #find time step index of peaks (during summer)
      sumPeakIndx<- apply(X= phytoConc[sumIndx,], MARGIN=2, FUN= which.max)
      
      #find peak values
      sumPeakConc<- apply(X= phytoConc[sumIndx,], MARGIN=2, FUN= max)
      
      #find peak time and convert to Day of Year
      sumPeakTime<- time[sumPeakIndx]
      names(sumPeakTime)<- colnames(phytoConc)
      sumPeakDOY<-  as.numeric(strftime(sumPeakTime, format = "%j"))
      names(sumPeakDOY)<- colnames(phytoConc)
      
      #fill summary values into matrices (write after each parameter loop)
      phytoMeanMat[v,]<- as.numeric(sumMeanConc)
      phytoPeakMat[v,]<- as.numeric(sumPeakConc)
      phytoPeakTimeMat[v,]<- as.numeric(sumPeakDOY)
      
      #create folder for simulation run (phytoName_parName_value)
      simFolderName<- paste(sep= "", phytoName, "_", parName, "_", newVal)
      cpath<- paste(sep= "", fpath, "/", simFolderName)
      dir.create(cpath)
      
      #write the time series file (write after each value loop)
      tsFile<- "phytoTimeSeries.csv"
      tsWrite<- cbind(as.character(time), phytoConc)
      write.csv(tsWrite, paste(sep= "", cpath, "/", tsFile), quote= FALSE, 
                row.names= FALSE)
      
      #print loop count if loop.counter== TRUE
      if(loop.counter== TRUE){
        loop.count<- loop.count + 1
        loop.status<- paste(sep= "", "Loop ", loop.count, "/", numLoops, " complete\n")
        cat(loop.status)
      }
      
    }#value loop
    
  #reset parameter back to initial value using ipvCount as index to parsInit 
  phytoNMLedit(filename= phytoFile, parName= parName, phytoName= phytoName, 
               newVal= parsInit[ipvCount])
    
  #create a summary folder for the phyto/parameter combination
  summaryFolder<- paste(sep= "", fpath, "/", phytoName, "_", parName, "_", "summary")
  dir.create(summaryFolder)
  
  #get parameter values
  parVals<- valueList[[p]]
  
  #add column names to summary matrices for indexing
  colnames(phytoPeakTimeMat)<- colnames(phytoPeakMat) <- colnames(phytoMeanMat)<- colnames(phytoConc)
  
  #Plot the parameter summary results versus the parameter values for major 
  #metrics (summer mean, summmer peak, summer peak timing)- 
  #Three plots and three CSV files for each phyto/par combination
  
  #Summer Mean biomass plot
  meanPlot<- paste(sep= "", summaryFolder, "/", phytoName, "_", parName, "_meanBiomass.png")
  ylabMean<- paste(sep= "", "Mean Summer ", phytoName, " Biomass (mg-C/m^3)")
  titleMean<- paste(sep= "", "Effect of ", parName, " parameter on ", phytoName)
  png(meanPlot)
  plot(x= parVals, y= (phytoMeanMat[,ncVarNames[y]]*12.01), xlab= "Parameter Value", 
       ylab= ylabMean, main= titleMean)  #12.01 to convert mmol-C/m^3 to displayed units
  dev.off()
  
  #Summer Peak biomass plot
  peakPlot<- paste(sep= "", summaryFolder, "/", phytoName, "_", parName, "_peakBiomass.png")
  ylabPeak<- paste(sep= "", "Peak Summer ", phytoName, " Biomass (mg-C/m^3)")
  titlePeak<- paste(sep= "", "Effect of ", parName, " parameter on ", phytoName)
  png(peakPlot)
  plot(x= parVals, y= (phytoPeakMat[,ncVarNames[y]]*12.01), xlab= "Parameter Value", 
       ylab= ylabPeak, main= titlePeak) #12.01 to convert mmol-C/m^3 to displayed units
  dev.off()
  
  #Summer Peak timing plot
  peakTimePlot<- paste(sep= "", summaryFolder, "/", phytoName, "_", parName, "_peaktime.png")
  ylabPeakTime<- paste(sep= "", "Time of Peak Summer ", phytoName, " Biomass (DOY)")
  titlePeakTime<- paste(sep= "", "Effect of ", parName, " parameter on ", phytoName)
  png(peakTimePlot)
  plot(x= parVals, y= phytoPeakTimeMat[,ncVarNames[y]], xlab= "Parameter Value",
       ylab= ylabPeakTime, main= titlePeakTime)
  dev.off()
    
  #CSV file for mean biomass (original GLM-FABM output units of mmol-C/m^3)
  meanCSV<- sub("png", "csv", x= meanPlot)
  writeMeanMat<- cbind(parVals, phytoMeanMat)
  write.csv(x= writeMeanMat, file= meanCSV, row.names= FALSE)
  
  #CSV file for peak biomass (original GLM-FABM output units of mmol-C/m^3)
  peakCSV<- sub("png", "csv", x= peakPlot)
  writePeakMat<- cbind(parVals, phytoPeakMat)
  write.csv(x= writePeakMat, file= peakCSV, row.names= FALSE)
  
  #CSV file for peak timing (units of DOY)
  peakTimeCSV<- sub("png", "csv", x= peakTimePlot)
  writePeakTimeMat<- cbind(parVals, phytoPeakTimeMat)
  write.csv(x= writePeakTimeMat, file= peakTimeCSV, row.names= FALSE)


  }#par loop

}#phyto loop


