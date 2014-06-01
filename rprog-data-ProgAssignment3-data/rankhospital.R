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
    #######################################################################
    ##  Relevant fields in outcome-of-care-measures.csv
    ##  field index field name
    ##   [2]     "Hospital.Name"
    ##   [7]     "State"
    ##   [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    ##   [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    ##   [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia" 
    
    ######################################################################## 
    
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
    
    if (outcome == validOutcomes[1]) { 
        OutcomeField <- 11
    } else if (outcome == validOutcomes[2]) {
        OutcomeField <- 17
    } else if (outcome == validOutcomes[3]){
        OutcomeField <- 23
    } else stop('invalid outcome')
    
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
    
    orderIndex<-order(statehospitals[,OutcomeField])
    
    #########################################################################
    # Create a ranking table consisting of 
    # hospital name, the value of the Outcome and the hospital rank
    #########################################################################
    
    rank <- vector('character')
    for (i in 1:length(orderIndex)){
        line <- c( statehospitals[orderIndex[i],2],
                   statehospitals[orderIndex[i],OutcomeField], i)
        # get all the subsequent compare the value of the Outcome field with the next one
        # if it equals it then sort by hospital name
        j <- i + 1
        while (j < length(orderIndex) && ){
            
        }
        rank <- c(rank, line)             
    }
    rankmatrix<-matrix(rank,3,nrow(statehospitals))
    
    if (num == 1 || num == "best"){
        # extract the names of all the hospitals with the 
        # best rank
        if 
        #extract the name of the best hospital
        statehospitals[orderIndex[1],c(2)]
        # sort the hospitals by name 
        order(rank)
    }
    #check if num == nrow since if it does then need to be careful
    # about handling ties
    
    
    
    
    
    
    ## Return hospital name in that state with the given rank
    ## 30-day death rate

    
    
}