outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character") 
## read csv data as character
outcome[,11] <- as.numeric(outcome[,11]) 
## coerce column "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" to numeric
hist(outcome[,11])
## plot histogram of column 11