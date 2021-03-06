rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  ## Check that state and outcome are valid
  possibleoutcomes = c("heart failure", "heart attack", "pneumonia") ## list of possible outcomes to check input
  if (outcome %in% possibleoutcomes == FALSE)
    stop("invalid outcome")
  
  if (outcome == "heart attack") { cause <- 11}
  if (outcome == "heart failure") { cause <- 17}
  if (outcome == "pneumonia") { cause <- 23}
  ## some clean up compared to best.R, since features 11,17 and 23 are used a lot, save if clauses
  outcomeDF[,cause] <- as.numeric(outcomeDF[,cause]) ## coerce feature to numeric
  allStates <- unique(outcomeDF$State)[order(unique(outcomeDF$State))]
  
  returnDF <- data.frame(row.names=c("hospital", "state"), stringsAsFactors=FALSE)
  ## Return hospital name in that state with the given rank 30-day death rate
  
  for (i in allStates) {
    rank <- num
    onestate <- outcomeDF[outcomeDF$State == i,] ## subset of data for one state
    
    if (is.integer(num) && num > length(onestate$Hospital.Name)) {return(NA)} 
    if (num == "best") { rank <- 1 }
    if (num == "worst") { rank <- length(order(onestate[,cause], na.last=NA))}
    
  
    
    rankID <- order(onestate[,cause], na.last=NA)[rank] ## maybe there is an easier way....
    rankValue <- onestate[,cause][rankID]             ## look for the Names of the IDs with the Value that corresponds to the order of num
    rankVect <- which(onestate[,cause] == rankValue)
  
    if (length(rankVect) > 1) {
      rankNames <- onestate$Hospital.Name[rankVect]
      rankNames <- rankNames[order(rankNames)] ## problem: which one do I return? e.g. looking for order num = 4, but 3 and 4 have the same value and have to be swapped alphabetically.... solution is...
      onestate$Hospital.Name[rankVect] <- rankNames  ## possibly evil: swap the names, if there is more than one result and the names are not sorted yet; should swap the entire row to avoid integrity issues, but in this case, it works
    }
  
  
    rank_order <- order(onestate[,cause],na.last=NA)[rank]
    name <- onestate$Hospital.Name[rank_order]
    tmp_df <- t(data.frame(c(name,i), stringsAsFactors=FALSE))
    returnDF <- rbind(returnDF, tmp_df)
  }  
  names(returnDF) <- c("hospital", "state")
  return(returnDF)
}