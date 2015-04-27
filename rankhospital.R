rankhospital <- function(state, outcome, num = "best") { 
  hospitalNameColumnNumber <- 2
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  states <- unique(outcomeData[, 7])
  numberOfRows <- nrow(outcomeData);
  if (! state %in% states) stop(paste("Error in rankhospital(", state, ", ", outcome, ") : invalid state", sep = "\""), call. = F)
  index <- if (num == "best") {
    1
  } else if (num == "worst") {
    numberOfRows
  } else if (!is.numeric(num)) {
    stop(paste("Error in rankhospital(", state, ", ", outcome, ", ", num, ") : invalid num", sep = "\""), call. = F)
  } else if (num > numberOfRows)  {
    NA
  } else {
    num
  }
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
        } else stop(paste("Error in rankhospital(", state, ", ", outcome, ") : invalid outcome", sep = "\""), call. = F)
  concreteStateSubset <- subset(outcomeData, State == state)
  orderedStateSubset <- order(concreteStateSubset[measuredColumnNumber], concreteStateSubset[hospitalNameColumnNumber], na.last = NA)
  index <- length(orderedStateSubset)
  rearrangedStateSubset <- concreteStateSubset[orderedStateSubset, ]
  rearrangedStateSubset[index, ]$Hospital.Name
}