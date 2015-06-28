## 2. Finding the best hospital in a state: 
# Write a function called best that take two arguments: the 2-character 
# abbreviated name of a state and an outcome name. The function reads the
# outcome-of-care-measures.csv file and returns a character vector with  the 
# name of the hospital that has the best (i.e. lowest) 30-day mortality for the 
# specified  outcome in that state. The hospital name is the name provided in 
# the Hospital.Name variable. The outcomes can be one of "heart attack", 
# "heart failure", or "pneumonia".  Hospitals that do not have data on a 
# particular outcome should be excluded from the set of hospitals when deciding 
# the rankings.
# 
## Handling ties
# If there is a tie for the best hospital for a given outcome, then the 
# hospital names should be sorted in alphabetical order and the frst hospital 
# in that set should be chosen (i.e. if hospitals "b", "c", and "f" are tied 
# for best, then hospital "b" should be returned).
# 
# The function should check the validity of its arguments. If an invalid state
# value is passed to best, the function should throw an error via the stop
# function with the exact message "invalid state". If an invalid outcome value 
# is passed to best, the function should throw an error via the stop function 
# with the exact message "invalid outcome".

best <- function(state, outcome) {
    
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that state is valid 
    # The instructions don't say whether to allow case insensitive, so I 
    # chose to allow it by forcing state to upper:
    state <- toupper(state)
    if (!any(state==outcomeData$State)) {stop("invalid state")}
    
    ## Check that the outcome is valid
    # Convert the outcome into something that looks like our column names: 
    expr <- paste("^Hospital.30.Day.Death..Mortality..Rates.from.",gsub(" ",".",outcome),sep="")
    # Get the index of a column matching that name:
    idx <- grep(expr, colnames(outcomeData), ignore.case = TRUE)
    # If there are no matches, the outcome was not found; if there is more than 
    # one match, the outcome was not specific enough (e.g.,"heart"):
    if (length(idx)!=1) {stop("invalid outcome")}

    ## Return hospital name in that state with lowest 30-day death
    ## rate
    
    # Coerce the column to numeric:
    outcomeData[,idx] <- as.numeric((outcomeData[,idx]))

    # Filter the data to non-NA values of the outcome in the state:
    outcomeData <- outcomeData[!is.na(outcomeData[,idx]) & outcomeData$State==state,]
    
    # Return the name of the first hospital in a list of hospitals with the 
    # lowest rate: 
    # Note: this could be done via 
    #    outcomeData$Hospital.Name[order(outcomeData[,idx],outcomeData$Hospital.Name)]
    # But this should be more efficient. 
    sort(outcomeData$Hospital.Name[outcomeData[,idx] == min(outcomeData[,idx])])
}