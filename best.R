best <- function(state, outcome) {
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
  # calculate min value
  numVector <- as.numeric(unlist(available[tcol]))
  minValue <- min(numVector)  # work variable of outcome numbers for repeat use
  # get rows for just min value. 
  minOnly <- available[numVector == minValue,]
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  # if more than 1 result is returned - choose 1st
  minOnly[order("Hospital.Name"),][1,"Hospital.Name"] 
}
