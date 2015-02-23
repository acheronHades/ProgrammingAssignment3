best <- function(state, outcome) {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  outcomeDF[,11] <- as.numeric(outcomeDF[,11]) ## coerce heart attack feature to numeric
  outcomeDF[,17] <- as.numeric(outcomeDF[,17]) ## coerce heart failure feature to numeric
  outcomeDF[,23] <- as.numeric(outcomeDF[,23]) ## coerce pneumonia feature to numeric
  
  oneresult <- function(vect=integer()) { ## if there is more than one best hospital the function reduces the output
    namevect <- character()
    for (i in vect) {
      name <- onestate$Hospital.Name[i] ## get all the names
      namevect <- append(namevect, name) ## create one name vector
    }
    namevect <- sort(namevect) ## sort vector alphabetically
    firstname <- namevect[1] ## get first entry
    firstID <- which(onestate$Hospital.Name == firstname) ## look up ID of first entry
    return(firstID)
  }
  
  ## Check that state and outcome are valid
  possibleoutcomes = c("heart failure", "heart attack", "pneumonia") ## list of possible outcomes to check input
  if (state %in% outcomeDF$State == FALSE) 
    stop("invalid state")
  if (outcome %in% possibleoutcomes == FALSE)
    stop("invalid outcome")
  
  ## Return hospital name in that state with lowest 30-day death rate
  onestate <- outcomeDF[outcomeDF$State == state,] ## subset of data for one state
  minHA <- min(na.omit(onestate[,11])) ## values for minimum death reates...
  minHF <- min(na.omit(onestate[,17]))
  minP <- min(na.omit(onestate[,23]))
  
  
  if (outcome == "heart attack")  ## which death rate is part of the input
    result <- which(onestate[,11] == minHA)
  if (outcome == "heart failure")
    result <- which(onestate[,17] == minHF)
  if (outcome == "pneumonia")
    result <- which(onestate[,23] == minP)
  
  if (length(result) > 1) ## more than one result -> reduce to alphabetically first name
    result <- oneresult(result)
  
  
  
  name <- onestate$Hospital.Name[result]
  print(name)
}