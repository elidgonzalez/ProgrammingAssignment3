rankhospital <- function(state, outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (state %in% hospitals$State) {
            hospitals <- hospitals[hospitals$State==state, ] 
      }
      else {
            stop('inavlid state')
      }
      if (outcome == 'heart attack') {
            hospitals <- hospitals[c('Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
            hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'] <- as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack']))
            hospitals <- hospitals[order(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack'], hospitals['Hospital.Name']), ]
            hospitals <- na.omit(hospitals)
      }
      else if (outcome == 'heart failure') {
            hospitals <- hospitals[c('Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
            hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'] <- as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure']))
            hospitals <- hospitals[order(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure'], hospitals['Hospital.Name']), ]
            hospitals <- na.omit(hospitals)
      }
      else if (outcome == 'pneumonia'){
            hospitals <- hospitals[c('Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
            hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'] <- as.numeric(unlist(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia']))
            hospitals <- hospitals[order(hospitals['Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia'], hospitals['Hospital.Name']), ]
            hospitals <- na.omit(hospitals)
      }
      else {
            stop('invalid outcome')
      }
      if (num == 'best') {
            num <- 1
      }
      else if (num == 'worst') {
            num <- nrow(hospitals)
      }
      else if (num > nrow(hospitals)) {
            return(NA)
      }
      else {
            num = num
      }
      hospitals$Hospital.Name[num]
}
