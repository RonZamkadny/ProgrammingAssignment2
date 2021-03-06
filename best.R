## heart attack is [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
## heart failure is [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
## pneumonia is [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
## hospital name [2] "Hospital.Name"
## state [7] "State"

best <- function(state, outcome){
  stateColumnNumber <- 7
  hospitalNameColumnNumber <- 2
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(outcomeData[, 7])
  if (! state %in% states) stop(paste("Error in best(", state, ", ", outcome, ") : invalid state", sep = "\""), call. = F)
  measuredColumnNumber <-
    if (outcome == "heart attack"){
      outcomeData [, 11] <- as.numeric(outcomeData[, 11])
      11
    } else
      if (outcome == "heart failure"){
        outcomeData [, 17] <- as.numeric(outcomeData[, 17])
        17
      } else
        if (outcome == "pneumonia"){
          outcomeData [, 23] <- as.numeric(outcomeData[, 23])
          23
        } else stop(paste("Error in best(", state, ", ", outcome, ") : invalid outcome", sep = "\""), call. = F)
  
  concreteStateSubset <- subset(outcomeData, State == state)
  minValue <- min(concreteStateSubset[measuredColumnNumber], na.rm = T)
  minRow <- subset(concreteStateSubset, concreteStateSubset[[measuredColumnNumber]] == minValue)
  minRow$Hospital.Name
}