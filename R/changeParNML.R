#changeParNML.R

#change parameters in an NML file

changeParNML<- function(filename, parName, newVal, phytoName=NULL, output= FALSE){
  
  #only one file at a time supported
  if(length(filename)!= 1){
    stop("Only one filename can be entered")
  }
  
  #check that parName and newVal are same length
  if(length(parName) != length(newVal)){
    stop("Length of parName vector must be equal to length of newVal vector")
  }
  
  #if phytoName exists, it should also be the same length as newVal
  if(!is.null(phytoName)){
    if(length(phytoName) != length(newVal)){
      if(length(phytoName) != 1){
        stop("Length of phytoName must be 1 (constant) or equal to length of newVal")
      } else if(length(phytoName== 1)){
        phytoName<- rep(phytoName, length(newVal))
      }
    }
  }
  
  #read in lines
  allLines<- readLines(con=filename)
  
  #number of pars to change
  pars<- length(newVal)
  oldVal<- numeric(pars)
  
  if(is.null(phytoName)){ #either glm.nml or fabm.nml file
    for(i in 1:pars){
      pat<- paste(sep= "", parName[i], "\\s*=")
      lindx<- grep(pattern= pat, x= allLines, ignore.case= TRUE)
      loi<- allLines[lindx]
      if(length(loi) > 1){
        stop(paste(sep= "", "More than one grep() match of ", pat," found"))
      }
      if(length(loi)== 0){
        stop(paste(sep= "", "No grep() match of ", pat, " found"))
      }
      oldVal[i]<- as.numeric(gsub(pattern= "\\D+=(.*)", replacement= "\\1", x= loi))
      rep<- paste(sep= "", "\\1 ", as.character(newVal[i]))
      loi<- sub(pattern= "(\\D+=).*", replacement= rep, x= loi)
      #overwrite lines
      allLines[lindx]<- loi
    }
    
    if(output== TRUE){
      info<- list(filename, parName, oldVal, phytoName, output)
      names(info)<- c("filename", "parName", "newVal", "phytoName", "output")
      actLines<- allLines[- grep(pattern= "\\s*!", x= allLines)]
      parLines<- grep(pattern= "\\s*\\w+\\s*=\\s*(\\d* | \\d*\\.\\d*).*", x= actLines, value= TRUE)
      parameter<- gsub(pattern= "\\s*(\\w+)\\s*=.*", x= parLines, replacement= "\\1")
      value<- gsub(pattern= "\\s*\\w+\\s*=\\s*(.*)", x= parLines, replacement= "\\1")
      value<- gsub(pattern= " ", replacement= "", x= value)
      out<- list(parameter, value, info)
      names(out)<- c("parameter", "value", "info")
    }
  } else { #aed_phyto_pars.nml
    
    #read in the lines
    allLines<- readLines(filename)
    
    #find section divider line index, and final line (starting with "/")
    sect<- grep(pattern= "^&", x= allLines)
    endln<- grep(pattern= "^/", x= allLines)
    
    #series of operations to extract clean column headers
    colHeadLine<- allLines[sect-1] #get line
    
    #define a function for seperating a data line
    sepLine<- function(lin, repl){
      for(i in 1:length(repl)){
      #replace anything thats not a word with a comma
      lin<- gsub(pattern= repl[i], replacement= ",", x= lin)
      #remove leading commas
      lin<- gsub(pattern= "^,*", replacement= "", x= lin)
      #replace repeated commons with a single comma
      lin<- gsub(pattern= ",+", replacement= ",", x= lin)
      }
      return(lin)
    }
    colHeadLine<- sepLine(colHeadLine, "\\W")
   
    #extract column names vector
    colNms<- unlist(strsplit(x= colHeadLine, split= ","))
    
    if(length(colNms) != 48){
      warnLine<- sect-1
      stop(paste(sep= "", "Column Names vector not of length 48; check Line ",
                 warnLine, " of ", filename))
    }
    
    #preallocate data matrix
    numPhytos<- (endln-sect)-1
    phytoData<- matrix(nrow= numPhytos, ncol= 48)
    
    #extract matrix of data values
    for(k in (sect+1):(endln-1)){
      splt<- unlist(strsplit(allLines[k], split= ","))
      if(length(splt) != 48){
        stop(paste(sep= "", "There are not 48 items in Line ", k, " of ", 
                   filename, "; \ncheck for comma errors"))
      }
      phytoData[k-sect, ]<- splt
    }
    
    #get phyto names
    phytoGroupNames<- gsub(pattern= ".*\\'(\\w+)\\'.*", replacement= "\\1", 
                         x= phytoData[,1])
    
    #fill in new values (search via grep with ignore.case= TRUE)
    for(i in 1:pars){
      cI<- grep(pattern= paste(sep= "", "\\<", parName[i], "\\>"), x= colNms, ignore.case= TRUE)
      if(length(cI)== 0){
        stop(paste(sep= "", "No column header match found for ", parName[i]))
      } else if(cI==1){
        stop("'p_name' is not a valid parName")
      }
      rI<- grep(pattern= phytoName[i], x= phytoData[,1], ignore.case= TRUE)
      if(length(rI)== 0){
        stop(paste(sep= "", "No phytoplankton name match found for ", phytoName[i]))
      }
      #overwrite value (include a tab)
      oldVal[i]<- as.numeric(phytoData[rI, cI])
      phytoData[rI, cI]<- paste(sep= "", "\t", as.character(newVal[i])) 
    }
    
    newLines<- character(numPhytos)
    #overwrite lines
    for(n in 1:numPhytos){
      newLines[n]<- paste(collapse= ",", as.character(phytoData[n,]))
    }
    allLines[(sect+1):(endln-1)]<- newLines
    
    if(output== TRUE){
      for(i in 1:numPhytos){
        for(j in 2:48){
          phytoData[i,j]<- gsub(pattern= "\\t", replacement= "", x= phytoData[i,j])
        }
      }
      info<- list(filename, parName, oldVal, phytoName, output)
      names(info)<- c("filename", "parName", "newVal", "phytoName", "output")
      out<- phytoData
      colnames(out)<- colNms
      out<- out[,-1]
      rownames(out)<- phytoGroupNames
      out<- list(out, info)
      names(out)<- c("phytoValues", "info")
    }
  }
  
  #rewrite the file
  writeLines(text= allLines, con= filename)
  
  if(output== TRUE){
    return(out)
  }
  
  
  
}