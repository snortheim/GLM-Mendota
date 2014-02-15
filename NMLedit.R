#NMLedit.R (function)
#February 7, 2014
#Craig A. Snortheim

#NMLedit(filename, parName, newVal, comment.char= "!", output= FALSE)
#
#Arguments:
#filename [character];text file to read with extension .txt, .nml, etc.; example: "glm.nml"
#parName [character]; exact parameter name; example: "coef_wind_drag"
#newVal [numeric]; value to which parName parameter will be set; example: 0.0016
#comment.char [character]; default is "!"; character indicating comments; example: "!" or "#"
#output [logical]; default is FALSE, which returns no output; if TRUE, function will
      #return a list object of parameter name, old value, and new value, for records
#Example usage:
  #NMLedit("glm.nml", "coef_wind_drag", 0.0016, "!", output= FALSE)

NMLedit<- function(filename, parName, newVal, comment.char= "!", output= FALSE){
  
  #check if the stringr package is installed;load
  if (!require("stringr")){
    install.packages("stringr")
    require("stringr")}
  
  #read in the file contents
  text<- readLines(filename, warn= FALSE)
  
  #find line locations where parName appears, ignorning case
  locs<- grep(parName, text, ignore.case= TRUE)
  
  #make sure parName was found at least once
  if(length(locs)==0){stop("Text string parName was not found in the specified file.")}
  
  #remove all locations that begin with the comment character
  comLines<- which(substr(text[locs], 1, 1)== comment.char)
  locs<- locs[-comLines]
  
  #remove lines where the parName string appears more than once
  badLines<- NULL
  for(i in 1:(length(locs))){
    charPos<- str_locate_all(pattern= parName, text[locs][i])
    if(nrow(as.data.frame(charPos)) > 1){badLines<- c(badLines, i)}
  }#end for loop
  if(is.null(badLines)== FALSE){
  locs<- locs[-badLines]}
  
  #remove lines in which the characters directly adjacent to the parName text string
  #are NOT spaces or equal sign (the target par line shoud pass these criteria)
  badLines2<- NULL
  for(j in 1:(length(locs))){
    charPos2<- str_locate_all(pattern= parName, text[locs][j])
    posStart<- as.data.frame(charPos2)[1,1]
    posEnd<- as.data.frame(charPos2)[1,2]
    lineSplit<- unlist(strsplit(text[locs][j], ""))
    if(posStart==1){
      if(lineSplit[posEnd+1] != " " & lineSplit[posEnd+1] != "="){
        badLines2<- c(badLines, j)}}
    if(posStart>1){
      if(lineSplit[posStart-1] != " " | lineSplit[posEnd+1] != " " & lineSplit[posEnd+1] != "="){
        badLines2<- c(badLines2, j)}}
    if(is.null(badLines2)== FALSE){
      locs<- locs[-badLines2]}
    }#end for loop
  
  #make sure that only 1 loc remains
  if(length(locs) > 1){stop("parName string appears more than once outside of comments")}
  
  #take single remaining loc as line of interest
  loi<- locs
  line<- text[loi]
  
  #split the line at the equal sign ("=") and take second component as old value
  oldVal<- as.numeric(unlist(strsplit(line, "="))[2])
  lead<- unlist(strsplit(line, "="))[1]
  
  #paste new value into the line text
  newLine<- paste(lead, "=", as.character(newVal), sep = "")
  
  #substitute newLine for old line
  text[loi]<- newLine
  
  #overwrite file with new value
  write(text, filename)
  
  #create return objects if output== TRUE
  if(output == TRUE){
    rawtextOut<- paste("Parameter: ", parName, "\nOld Value: ", oldVal, "\nNew Value: ", newVal)
    cat(rawtextOut)
  }
  
}#end function




