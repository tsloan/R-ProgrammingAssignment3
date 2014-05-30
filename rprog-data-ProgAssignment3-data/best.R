best <- function(state, outcome) {
  ## Check that state and outcome are valid
  ## check they are not missing
  if (missing(outcome)) stop("invalid outcome")
  if (missing(state)) stop ("invalid state")

  # above still does not catch an undefined object  
  
  #######################################################################
  ##  Relevant fields in outcome-of-care-measures.csv
  ##  field index field name
  ##   [2]     "Hospital.Name"
  ##   [7]     "State"
  ##   [15] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ##   [21] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ##   [27] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
  
  ######################################################################## 
  # Check the request outcome is valid
  validOutcomes <-c("heart attack", "heart failure", "pneumonia")
  goodOutcomes <- (validOutcomes == outcome)
  # goodOutcomes is a boolean list indicating if outcome is in the
  # validOutcomes list
  # if outcome is in the list then the sum of the booleans
  # in goodOutcomes will be 1.
  if (sum(goodOutcomes) == 0) stop('invalid outcome')    
  
  if (outcome == validOutcomes[1]) MortalityField <- 15
  else if (outcome == validOutcomes[2]) MortalityField <- 21
  else if (outcome == validOutcomes[3]) MortalityField <- 27
  else stop('invalid outcome')
  
  #########################################################################
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
  
  #########################################################################
  # Check the state is valid
  # The two letter state code is held in field 7 of 
  # outcome-of-care-measures.csv
  USstate <- 7
  
  # create table of the number of hospitals in each state
  # then use names() to get the 2 letter statecode
  stateCodes<-names(table(data[USstate]))
  
  # check the state input variable against the
  # stateCodes
  goodStates <- (state == stateCodes)
  if (sum(goodStates) == 0) stop('invalid state')    
    
  #######################################################################
  ## find the name of the hospital that has the best (i.e. lowest) 
  ## 30-day mortality for the specified outcome in that state.
  ##
  
  ## get the hospitals in state
  statehospitals <- data[data[USstate]==state,]
  # extract the hospital name and the mortality rates for the 
  # desired outcome
  statehospitals[,c(2,MortalityField)] 
  #  sort these into ascending order by Mortality field, ignoring NAs
  
  
  ## Return hospital name in that state with lowest 30-day death
  ## rate
}