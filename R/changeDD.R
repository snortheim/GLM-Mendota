#changeMetDD

#driverVar- must match a column header in the filename file
#operation- one of c("+", "*")
#value- numeric value to be applied by operation (may be negative)
#tod- time of day to apply operation - c("day", "night", "all")
#output- if TRUE, original data will be returned in a data frame object

changeDD<- function(filename, driverVar, operation, value, tod, output= FALSE){
  
  #CHECKS
  if(length(filename) != 1){
    stop("Only one file can be listed in filename argument")
  }
  if(length(unique(c(length(driverVar), length(operation), length(value), length(tod))))!= 1){
    stop("Arguments driverVar, operation, value and tod must have same length")
  } else{
    n<- length(driverVar)
  }
  
  #read in the data (warn if extension is not txt- datetimes will be written incorrectly)
  if(as.character(unlist(strsplit(x= filename, split= "\\.")))[2] != "txt"){
    warning("DateTime formats are not written correctly to CSV files; see \n
            changeDD help page for more inforation")
  }
  defData<- read.csv(file= filename)
  defData[,1]<- as.character(defData[,1])
  colHeaders<- names(defData)
  
  #save old driver data if output is TRUE
  if(output== TRUE){
    origData<- defData
  }
  
  #define day/night identifier helper function
  #night defined as SW rad < 10 W/m2
  dayNightFind<- function(metData){
    allIndx<- 1:(nrow(metData))
    nightIndx<- which(metData[,2] < 10)
    dayIndx<- which(metData[,2] >= 10)
    todIndx<- list(dayIndx, nightIndx, allIndx)
    names(todIndx)<- c("day", "night", "all")
    return(todIndx)
  }
  
  #loop for each variable
  for(i in 1:n){
    #find variable column of interest
    driverVarIndx<- grep(pattern= paste(sep= "", "^", driverVar[i]), x= colHeaders, 
                      ignore.case= TRUE)
    
    #MORE CHECKS
    if(length(driverVarIndx)== 0){
      stop(paste(sep= "", "No column header match found for '", driverVar[i]), "'")
    }
    if(length(grep(pattern= tod[i], x= c("day", "night", "all"), ignore.case= TRUE)) != 1){
      stop("Each element of tod argument must be one of 'day', 'night' or 'all'")
    }
    if(length(grep(pattern= paste(sep= "", "\\", operation[i]), x= c("+", "*"))) != 1){
      stop("Each element of operation argument must be one of '+' or '*'")
    }
    
    #get row indicies using character indexing of tod argument
    rIndx<- dayNightFind(defData)[[tod[i]]]
    
    #parse and evulate the scaling expression
    defData[,driverVarIndx][rIndx]<- eval(parse(text= paste(sep= "", "defData[,driverVarIndx][rIndx]", operation[i], value[i])))
    
  }
  
  #write new file
  write.table(x= defData, file= filename, quote= FALSE, sep= ",", row.names= FALSE)
  
  #return original data if output is TRUE
  if(output== TRUE){
    return(origData)
  }
}