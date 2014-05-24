best <- function(state,outcome) {
    ## Read outcome data
    data<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        
    ## Check that state and outcome are valid
    
    outcome <- 'test'
    validOutcomes <-c("heart attack", "heart failure", "pneumonia")
    for (i in length(validOutcomes)){
        if (validOutcomes[i] != outcome ){
            stop('invalid outcome')    
        }            
    }
    
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
}