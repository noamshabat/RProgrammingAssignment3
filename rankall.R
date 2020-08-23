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
  available <- outcomeDF[outcomeDF[tcol] != "Not Available", 
                         c("State","Hospital.Name", tcol)]
  
  ret <- data.frame(hospital=character(), state=character())
  #names(ret) <- c("hospital", "state")
  ## For each state, find the hospital of the given rank
  for (state in unique(available$State)) {
    # get ordered data for current state
    dState <- available[available$State == state,]
    dState <- dState[order(as.numeric(unlist(dState[tcol])),dState$Hospital.Name),]
    
    # normalize rank to number
    ranks <- length(dState$Hospital.Name)
    if (num == "best") {
      num = 1
    } else if (num == "worst") {
      num = ranks
    } 
    
    ret <- rbind(ret, c("hospital"=dState[num,"Hospital.Name"],"state"=state))
  }
  # rbind messes up col names for some reason. fix it.
  names(ret) <- c('hospital','state')
  row.names(ret) <- ret$state
  
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  ret[order(ret$state),]
}
