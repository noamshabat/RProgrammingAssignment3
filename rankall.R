rankall <- function(outcome, num = "best") {
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

  # get only relevant data - hospital name and outcome. filter not avialable.
  tcol <- validOutcomes[outcome]
  available <- outcomeDF[stateData[tcol] != "Not Available", 
                         c("Hospital.Name", tcol)]
  
  # normalize rank to number
  ranks <- length(unique(available$Hospital.Name))
  if (num == "best") {
    num = 1
  } else if (num == "worst") {
    num = ranks
  } else if (num > ranks) {
    return(NA)
  }
  
  ret <- data.frame(row.names = c("State","Hospital.Name"))
  ## For each state, find the hospital of the given rank
  for (state in unique(available$State)) {
    dState <- available["State" == state]
    dState <- dState[order(dState[tcol],dState$Hospital.Name)]
    
  }
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
}
