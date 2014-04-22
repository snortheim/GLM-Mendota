#phytoNMLedit.R
#Craig A Snortheim
#March 27, 2014

phytoNMLedit<- function(filename, parName, phytoName, newVal, output= FALSE){
  
  #read in the text
  text<- readLines(filename, warn= FALSE)
  
  #search for the line that has the column headers
  colHeadIndx<- grep("p_name(\\s+ | \\t+)p_initial", ignore.case= FALSE, x= text)
  
  #use index to extract line
  colHeaders<- text[colHeadIndx]
  
  #replace anything that's not a word with a comma
  csvColHeaders<- gsub("(\\W+)", replacement= ",", x= colHeaders)
  
  #eliminate the leading comma
  csvColHeaders<- sub(",", replacement= "", x=csvColHeaders)
  
  #separate col headers into vector
  headers<- unlist(strsplit(csvColHeaders, ","))
  
  #determine number of data lines and starting location for loop
  nlines<- length(text) - 1 #last line is simply "/"
  startLine<- colHeadIndx+3 #following line is &phyto_data, and first line is unique
  
  #comma-separate first data line (with "pd=")
  firstLine<- gsub("(\\t+)", replacement= ",", x=text[startLine-1]) #replace tabs
  firstLine<- gsub("(\\s+)", replacement= ",", x= firstLine) #replace spaces
  firstLine<- sub(".*=,", replacement= "", x=firstLine) #elminate pd text and =
  firstLine<- gsub(",+", replacement= ",", x= firstLine) #replace any duplicate commas with single comma
  firstLine<- sub(",*$", replacement= "", x=firstLine) #eliminate trailing commas
  firstLine<- gsub("'", replacement= "", x=firstLine) #eliminate single quotes
  
  #separate firstLine into vector
  dl1<- unlist(strsplit(firstLine, ",")) #split string on comma
  pn1<- dl1[1] #phyto name is in position 1
  
  #set up data.frame for all phyto data, put in row 1
  nrow<- nlines-startLine + 2
  ncol= length(dl1)
  phytoData<- matrix(nrow= nrow, ncol= ncol)
  colnames(phytoData)<- headers
  phytoData[1,]<- dl1 #fill line 1
  
  ll<- 2 #start line count
  #do same for all remaining data line (all identical from here to end)
  for(i in startLine:nlines){
    expandStr<- gsub("(\\t+)", replacement= ",", x=text[i]) #replace tabs
    expandStr<- gsub("(\\s+)", replacement= ",", x= expandStr) #replace spaces
    expandStr<- sub("^,*", replacement= "", x= expandStr) #elminate leading commas
    expandStr<- gsub(",+", replacement= ",", x= expandStr) #replace any duplicate commas with single comma
    expandStr<- sub(",*$", replacement= "", x=expandStr) #eliminate trailing commas
    expandStr<- gsub("'", replacement= "", x=expandStr) #eliminate single quotes
    
    dlc<- unlist(strsplit(expandStr, ","))
    phytoData[ll,]<- dlc 
    ll<- ll+1 #count
  }
  
  #now find the target row and target column and change to newVal
  targetRow<- which(phytoData[,1]== phytoName)
  targetCol<- which(colnames(phytoData)== parName)
  phytoData[targetRow, targetCol]<- newVal
  
  #find line index of adjusted line using phytoName
  loi<- grep(phytoName, x= text)
  newLine<- phytoData[targetRow,]
  names(newLine)<- NULL
  buildText<- NULL
  
  #build new text line
  nll<- length(newLine)
  for(t in 1:(nll-1)){
    buildText<- paste(sep ="", buildText, newLine[t], ",\t") 
  }
  buildText<- paste(sep= "", buildText, newLine[nll], ",")
  
  #first first line, add leading "pd= ", if other, add leading \t
  if(loi== (startLine-1)){
    buildText<- paste(sep= "", "pd = ", buildText)
  } else {
    buildText<- paste(sep= "", "\t", buildText)
  }
  
  #sub single quotes back into the phytoName
  qqphytoName<- paste(sep= "", "'", phytoName, "'")
  buildText<- sub(phytoName, replacement= qqphytoName, buildText)
  
  #replace line of interest with newly constructed line
  text[loi]<- buildText
  
  #re-write the file with new line
  writeLines(text= text, con= filename)
  
  #if output is true, return the matrix phytoData *(with newVal)
  if(output== TRUE){
    return(phytoData)
  }
  
  
}#end function
