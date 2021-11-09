best <- function(state, outcome) {
      hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (state %in% hospitals$State) {
            hospitals <- hospitals[hospitals$State==state, ] 
      }
      else {
            stop('inavlid state')
      }
      if (outcome == 'heart attack') {
            x <- which.min(as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'])))
      }
      else if (outcome == 'heart failure') {
            x <- which.min(as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'])))
      }
      else if (outcome == 'pneumonia'){
            x <- which.min(as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'])))
      }
      else {
            stop('invalid outcome')
      }
      hospitals$Hospital.Name[x]
      ## Read outcome data
      ## Return hospital name in that state with lowest 30-day death
      ## rate
}
## Check that state and outcome are valid
