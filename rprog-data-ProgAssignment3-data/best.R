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
  ##   [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
  ##   [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
  ##   [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
  
  ######################################################################## 
  # Check the request outcome is valid
  validOutcomes <-c("heart attack", "heart failure", "pneumonia")
    
  if (outcome == validOutcomes[1]) MortalityField <- 11
  else if (outcome == validOutcomes[2]) MortalityField <- 17
  else if (outcome == validOutcomes[3]) MortalityField <- 23
  else stop('invalid outcome')
  
  #########################################################################
  ## Read outcome data
  data<-read.csv("outcome-of-care-measures.csv", 
                  colClasses="character", 
                  na.strings=c("Not Available","NA"))
  
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
  
  # coerce outcome measurement from character to numeric
  statehospitals[, MortalityField] <- as.numeric(statehospitals[, MortalityField])
  
  #  Get the minimum value for the Mortality for that outcome, ignoring NAs 
  MinMortality <- min(statehospitals[,MortalityField],na.rm=TRUE)
  bad <- is.na(statehospitals[MortalityField])
  statehospitals <-statehospitals[!bad,] 
  # Get the hospitals with the lowest mortality    
  MinHospitals<-statehospitals[MortalityField]==MinMortality 
  lowhospitals<-statehospitals[MinHospitals,c(2)]
  if (length(lowhospitals)>1){
      lowhospitals<-min(lowhospitals)
  }

  ## Return hospital name in that state with lowest 30-day death
  ## rate
  lowhospitals
  
}