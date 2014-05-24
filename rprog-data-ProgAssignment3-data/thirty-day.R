# plot the 30-day mortality rates for heart attack
#
outcome<-read.csv("outcome-of-care-measures.csv", colClasses="character")
outcome[,11] <-as.numeric(outcome[,11])
hist(outcome[,11])
