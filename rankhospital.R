rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeDF <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  validOutcomes <- c(
    "heart attack"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",
    "heart failure"="Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",
    "pneumonia"="Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  )
  
  if (is.na(validOutcomes[outcome])) {
    stop("invalid outcome")
  }
  if (is.na(table(outcomeDF$State)[state])) {
    stop("invalid state")
  }
  
  # get only relevant data first - hospital name and outcome for specific state.
  tcol <- validOutcomes[outcome]
  stateData <- outcomeDF[outcomeDF$State==state, c("Hospital.Name", tcol)]
  
  # remove hospitals with no data
  available <- stateData[stateData[tcol] != "Not Available",]
  
  # normalize rank to number
  ranks <- length(unique(available$Hospital.Name))
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = ranks
  } else if (num > ranks) {
    return(NA)
  }
  
  # sort data frame by outcome and name
  tcolNumeric = as.numeric(unlist(available[tcol]))
  sorted = available[order(tcolNumeric, available$Hospital.Name),]
  
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  sorted[num,"Hospital.Name"]
}
