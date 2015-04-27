rankall <- function(outcome, num = "best") {
  hospitalNameColumnNumber <- 2
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  numberOfRows <- nrow(outcomeData)
  index <- if (num == "best") {
    1
  } else if (!is.numeric(num) && num != "worst") {
    stop(paste("Error in rankhospital(", outcome, ", ", num, ") : invalid num", sep = "\""), call. = F)
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
        } else stop(paste("Error in rankhospital(", outcome, ") : invalid outcome", sep = "\""), call. = F)
  
  outcomeSplittedByState <- split(outcomeData, outcomeData$State)
  f <- lapply(outcomeSplittedByState, function(x) {
    hospitals <- x[order(x[measuredColumnNumber], x[hospitalNameColumnNumber], na.last = NA), ][hospitalNameColumnNumber]$Hospital.Name
    index <- if (num == "worst") {
      length(hospitals)
    } else index 
    hospitals[index]
  })
  resultDataFrame <- data.frame(hospital=unlist(f), state=names(f))
  row.names(resultDataFrame) <- NULL
  resultDataFrame
}