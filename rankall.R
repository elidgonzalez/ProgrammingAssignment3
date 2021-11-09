rankall <- function(outcome, num = "best") {
      ## Read outcome data
      ## Check that state and outcome are valid
      ## Return hospital name in that state with the given rank
      ## 30-day death rate
      hospitals <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
      if (outcome == 'heart attack') {
            hospitals <- hospitals[c('State', 'Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack')]
            hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack <- as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
            hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack, hospitals$Hospital.Name), ]
      } 
      else if (outcome == 'heart failure') {
            hospitals <- hospitals[c('State', 'Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure')]
            hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure <- as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
            hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure, hospitals$Hospital.Name), ]
      }
      else if (outcome == 'pneumonia') {
            hospitals <- hospitals[c('State', 'Hospital.Name', 'Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia')]
            hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia <- as.numeric(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
            hospitals <- hospitals[order(hospitals$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia, hospitals$Hospital.Name), ]
      } 
      else {
             stop('invalid outcome')
      }
      hospitals <- na.omit(hospitals)
      States = sort(unique(hospitals$State))
      hospitals <- split(hospitals, hospitals$State)
      hospitals <- setNames(hospitals, States)
      df = data.frame()
      for (state in States) {
            tmp <- 0
            tmp_df <- as.data.frame(hospitals[state])
            row.names(tmp_df) <- NULL
            if (num == 'best') {
                  tmp <- 1
            }
            else if (num == 'worst') {
                  tmp <- nrow(tmp_df)
            }
            else if (num > nrow(tmp_df)) {
                  tmp <- NA
            }
            else {
                  tmp = num
            }
            if (is.na(tmp)) {
                  df <- rbind(df, c(NA, state))
            }
            else {
                  df <- rbind(df, c(tmp_df[2][tmp, ], state))
            }
      }
      colnames(df) <- c('hospital', 'state')
      rownames(df) <- States
      df
}
