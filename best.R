best <- function(state, outcome) {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcomeDF[,11] <- as.numeric(outcomeDF[,11]) ## coerce heart attack feature to numeric
  outcomeDF[,17] <- as.numeric(outcomeDF[,17]) ## coerce heart failure feature to numeric
  outcomeDF[,23] <- as.numeric(outcomeDF[,23]) ## coerce pneumonia feature to numeric
  
  oneresult <- function(vect=integer()) {
    namevect <- character()
    for (i in vect) {
      name <- onestate$Hospital.Name[i]
      namevect <- append(namevect, name)
    }
    namevect <- sort(namevect)
    firstname <- namevect[1]
    firstID <- which(onestate$Hospital.Name == firstname)
    return(firstID)
  }
  
  ## Check that state and outcome are valid
  possibleoutcomes = c("heart failure", "heart attack", "pneumonia") ## list of possible outcomes to check input
  if (state %in% outcomeDF$State == FALSE) 
    stop("invalid state")
  if (outcome %in% possibleoutcomes == FALSE)
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  onestate <- outcomeDF[outcomeDF$State == state,]
  minHA <- min(na.omit(onestate[,11]))
  minHF <- min(na.omit(onestate[,17]))
  minP <- min(na.omit(onestate[,23]))
  
  
  if (outcome == "heart attack")
    result <- which(onestate[,11] == minHA)
  if (outcome == "heart failure")
    result <- which(onestate[,17] == minHF)
  if (outcome == "pneumonia")
    result <- which(onestate[,23] == minP)
  
  if (length(result) > 1) 
    result <- oneresult(result)
  
  
  
  name <- onestate$Hospital.Name[result]
  print(name)
}