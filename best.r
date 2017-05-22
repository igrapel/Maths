best <- function(state, outcome) {
  data <- read.csv("outcome-of-care-measures.csv", colClasses = "character", na.strings="Not Available")
  if(!is.element(state, data[, "State"])) {
    stop("invalid state")
  }
  data_subset <- data[which(data[ ,"State"] %in% state), ]     
  data_subset2 <- data_subset[complete.cases(data_subset), ]
  
  if(outcome=="heart attack") {                          
    HA <- data_subset2[order(as.numeric(as.character((data_subset2[,11]))), data_subset2[,2]),]                          
    HA[1,2]             
  } else if (outcome== "pneumonia") {                                               
    PN <- data_subset2[order(as.numeric(as.character((data_subset2[,23]))), data_subset2[,2]),]                          
    PN[1,2]             
  } 
  else if (outcome== "heart failure") {                  
    HF <- data_subset2[order(as.numeric(as.character((data_subset2[,17]))), data_subset2[,2]),]                 
    HF[1,2]             
  }   else stop("invalid outcome")
}