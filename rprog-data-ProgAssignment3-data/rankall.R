
#############################################################################
##
## rankall: takes two arguments: an outcome name (outcome) and a hospital 
##          ranking (num). The function reads the 
##          outcome-of-care-measures.csv file and returns a 2-column data 
##          frame containing the hospital in each state that has the ranking 
##          specified in num.
#############################################################################

rankall <- function(outcome, num = "best") {
    
    #############################################################################
    ##  rankStateHospital: An internal function for rankall.  This gets the 
    ##                     name of the hospital with the desired rank for the 
    ##                     specified outcome.
    ##                     It uses the data data frame from the surrounding
    ##                     environment
    #############################################################################
    
    rankStateHospital <- function(state, OutcomeField, num = "best"){
        
        #########################################################################
        ## Input parameters are not checked since they have already been
        ## checked by rankhospital
        #########################################################################
        
        # The two letter state code is held in field 7 of 
        # outcome-of-care-measures.csv
        USstate <- 7
        
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
        
        if (is.numeric(num) && num > nrow(statehospitals)) return (NA)
        
        #########################################################################
        # rank the remaining hospitals according to the values in the OutcomeField
        # and handle ties by sorting on the hospital name
        #########################################################################
        
        ranklist<-statehospitals[order(statehospitals[OutcomeField],
                                       statehospitals[2]), ]
        
        ##########################################################################
        ## Return hospital name in that state with the given rank
        ## 30-day death rate
        #
        # Return the name of the hospital with request rank in num
        ##########################################################################
        
        if (num == 1 || num == "best"){
            return(ranklist[1,c(2)])
            
        } else if (num == "worst" || num == nrow(ranklist)){
            return(ranklist[nrow(ranklist), c(2)])
        } else {
            return(ranklist[num,c(2)])
        }
        
        
    } # end of rankStateHospital function
    

    #########################################################################
    ## START OF rankall
    #########################################################################
    
        
    #########################################################################
    ## Check that outcome is not missing
    #########################################################################
    
    if (missing(outcome)) stop("invalid outcome")
        
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
    ## Extract the two letter codes for each state.
    #########################################################################
    
    # The two letter state code is held in field 7 of 
    # outcome-of-care-measures.csv
    USstate <- 7
    
    # create table of the number of hospitals in each state
    # then use names() to get the 2 letter statecode
    stateCodes<-names(table(data[USstate]))
    

    #########################################################################
    ## Loop over the stateCodes and for each stateCode, extract the
    ## name of the hospital with the desired rank. Add that hospital name,
    ## its statecode to the output data frame,
    ##
    ## Call a function that works on the existing data frame (note
    ## that R environments makes sure that the correct data frame is
    ## is used.)
    ## 
    #########################################################################

    hospitalname <- character(0) # initialise the vector of hospital names
        
    for (i in 1:length(stateCodes))
    {
        hospitalname <- c( hospitalname, 
                           rankStateHospital(stateCodes[i], OutcomeField, num))
    }
    
    #########################################################################
    ## Create the output data frame to hold the output data
    #########################################################################
    
    data.frame(hospital=hospitalname,state=stateCodes)
        
    
    ## For each state, find the hospital of the given rank
    ## Return a data frame with the hospital names and the
    ## (abbreviated) state name
}