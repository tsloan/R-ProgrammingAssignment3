#############################################################################
##
## rankhospital : The function reads the outcome-of-care-measures.csv 
##    file and returns a character vector with the name of the hospital 
##    that has the ranking specified by the num argument.
##    It takes three arguments: 
##       state: the 2-character abbreviated name of a state 
##       outcome:
##       num: the ranking of a hospital in that state for the outcome.
## 

rankhospital <- function(state, outcome, num = "best") {
    
    #########################################################################
    ## Check that state and outcome are are not missing
    #########################################################################

    if (missing(outcome)) stop("invalid outcome")
    if (missing(state)) stop ("invalid state")
        
    #########################################################################
    ## Read outcome data from csv file as characters.  Turn fields containing
    ## the string "Not Avaialble" in NAs
    #########################################################################
    
    data<-read.csv("outcome-of-care-measures.csv", 
                   colClasses="character", 
                   na.strings=c("Not Available","NA"))
    
    #########################################################################
    ## Check the state is valid 
    #########################################################################
    
    # The two letter state code is held in field 7 of 
    # outcome-of-care-measures.csv
    USstate <- 7
    
    # create table of the number of hospitals in each state
    # then use names() to get the 2 letter statecode
    stateCodes<-names(table(data[USstate]))
    
    # check the state input variable against the stateCodes
    goodStates <- (state == stateCodes)
    if (sum(goodStates) == 0) stop('invalid state') 
        
    #########################################################################
    ## Check the outcome is valid 
    #########################################################################
    
    validOutcomes <-c("heart attack", "heart failure", "pneumonia")
    
    if (outcome == validOutcomes[1]) OutcomeField <- 11
    else if (outcome == validOutcomes[2]) OutcomeField <- 17
    else if (outcome == validOutcomes[3]) OutcomeField <- 23
    else stop('invalid outcome')
    
    #########################################################################
    ## Check that num is valid 
    #########################################################################
    
    if (!is.numeric(num) && num != "best" && num != "worst") {
            stop ("invalid num")  
    }
    if (num <= 0) stop ("invalid num")
        
    #########################################################################
    ## Extract the rows for the hospitals in state 
    #########################################################################
    
    statehospitals <- data[data[USstate]==state,]
    
    # coerce outcome measurement from character to numeric
    statehospitals[, OutcomeField] <- 
        as.numeric(statehospitals[, OutcomeField])
    
    # remove the hospitals where value for the outcome field is NA
    bad <- is.na(statehospitals[OutcomeField])
    statehospitals <-statehospitals[!bad,]
    
    ## If num is less than the number of hospitals left, exit from the function
    ## with NA as the return value
    
    if (num > nrow(statehospitals)) return (NA)
    
    #########################################################################
    # rank the remaining hospitals according to the values in the OutcomeField
    # in ascending order
    #########################################################################
    
    order(statehospitals[,OutcomeField])
    
    
    
    
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    
    
}