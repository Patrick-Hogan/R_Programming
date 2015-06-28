## 3  Ranking hospitals by outcome in a state
# Write a function called rankhospital that takes three arguments: 
#     the 2-character abbreviated name of a state (state), 
#     an outcome (outcome), 
#     the ranking of a hospital in that state for that outcome (num)
# The function reads the outcome-of-care-measures.csv file and returns a 
# character vector with the name of the hospital that has the ranking specified 
# by the num argument. For example, the call 
#     rankhospital("MD", "heart failure", 5)
# would return a character vector containing the name of the hospital with the 
# 5th lowest 30-day death rate for heart failure. The num argument can take 
# values "best", "worst" or an integer indicating the ranking (smaller numbers 
# are better). If the number given by num is larger than the number of hospitals 
# in that state, then the function should return NA. Hospitals that do not have 
# data on a particular outcome should be excluded from the set of hospitals when 
# deciding the rankings. 
# 
## Handling ties.  
# It may occur that multiple hospitals have the same 30-day mortality rate for a 
# given cause of death.  In those cases ties should be broken by using the 
# hospital name.  For example, in Texas ("TX"), the hospitals with lowest 30-day 
# mortality rate for heart failure are shown here.
# > head(texas)
#     Hospital.Name Rate Rank
#     3935       FORT DUNCAN MEDICAL CENTER  8.1    1
#     4085  TOMBALL REGIONAL MEDICAL CENTER  8.5    2
#     4103 CYPRESS FAIRBANKS MEDICAL CENTER  8.7    3
#     3954           DETAR HOSPITAL NAVARRO  8.7    4
#     4010           METHODIST HOSPITAL,THE  8.8    5
#     3962  MISSION REGIONAL MEDICAL CENTER  8.8    6
# Note that Cypress Fairbanks Medical Center and Detar Hospital Navarro both 
# have the same 30-day rate (8.7). However, because Cypress comes before Detar
# alphabetically, Cypress is ranked number 3 in this scheme and Detar is ranked 
# number 4. One can use the order function to sort multiple vectors in this 
# manner (i.e. where one vector is used to break ties in another vector).
# 
# The function should check the validity of its arguments.  If an invalid state
# value is passed to best,  the function should throw an error via the stop 
# function with the exact message "invalid state".  If an invalid outcome value 
# is passed to best, the function should throw an error via the stop function 
# with the exact message "invalid outcome".

rankhospital <- function(state, outcome, num = "best") {
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
    
    ## Return hospital name in that state with the given rank 30-day death rate
    
    # Coerce the column to numeric:
    outcomeData[,idx] <- as.numeric((outcomeData[,idx]))
    
    # Filter the data to non-NA values of the outcome in the state:
    outcomeData <- outcomeData[!is.na(outcomeData[,idx]) & outcomeData$State==state,]
    
    # If num was "best", make it 1; if it was "worst", make it 
    # nrow(outcomeData); if it was greater than nrow(outcomeData), return NA
    num <- if (is.character(num)) {
            switch(tolower(num), best = 1, worst = nrow(outcomeData), NA)
            } else {num
            }
    if (all(num > nrow(outcomeData))) {return(NA)}
    
    # Order the table and index it:
    outcomeData[order(outcomeData[,idx],outcomeData$Hospital.Name),"Hospital.Name"][num]

}