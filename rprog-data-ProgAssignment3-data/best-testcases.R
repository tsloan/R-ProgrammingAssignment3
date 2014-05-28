##
## test Cases for best.R
#

# missing state
best(outcome = "heart attack")

# missing outcome
best(state = "TX")

# undefined  state and outcome tests
#outcome = "heart attack"
# best (state, outcome)
# need to clear the outcome object
# state ="TX"
# best (state, outcome)


# outcome not one of the required outcomes
outcome <- "test"
state <-"TX"
best(state, outcome)

# outcome is valild, but invalide state
outcome <- "heart attack"
state <- "ZZ"
best(state, outcome)

