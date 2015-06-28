## 3  Ranking hospitals in all states
# Write a function called rankall that takes two arguments: 
#     an outcome (outcome), 
#     the ranking of a hospital for that outcome (num)
# The function reads the outcome-of-care-measures.csv file and returns a 
# 2-column data frame containing the hospital in each state that has the ranking 
# specified in num. For example the function call 
#     rankall("heart attack", "best")
# would return a data frame containing the names of the hospitals that are the 
# best in their respective states for 30-day heart attack death rates.  The 
# function should return a value for every state (some may be NA). The first 
# column in the data frame is named hospital, which contains the hospital name, 
# and the second column is named state, which contains the 2-character 
# abbreviation for the state name. Hospitals that do not have data on a 
# particular outcome should be excluded from the set of hospitals when deciding
# the rankings.
# # Handling ties
# The rankall function should handle ties in the 30-day mortality rates in the 
# same way that the rankhospital function handles ties.
# 
# The function should check the validity of its arguments. If an invalid 
# outcome value is passed to best, the function should throw an error via the 
# stop function with the exact message "invalid outcome".

rankall <- function(outcome, num = "best") {
    ## Read outcome data
    outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Check that the outcome is valid
    # Convert the outcome into something that looks like our column names: 
    expr <- paste("^Hospital.30.Day.Death..Mortality..Rates.from.",gsub(" ",".",outcome),sep="")
    # Get the index of a column matching that name:
    idx <- grep(expr, colnames(outcomeData), ignore.case = TRUE)
    # If there are no matches, the outcome was not found; if there is more than 
    # one match, the outcome was not specific enough (e.g.,"heart"):
    if (length(idx)!=1) {stop("invalid outcome")}
    
    ## Return hospital name in each state with the given rank 30-day death rate
    
    # Coerce the column to numeric:
    outcomeData[,idx] <- as.numeric((outcomeData[,idx]))
    
    # Filter the data to non-NA values of the outcome:
    outcomeData <- outcomeData[!is.na(outcomeData[,idx]),]
    
    
    getIndex <- function(x,num) {
        # If num was "best", make it 1; if it was "worst", make it 
        # nrow(outcomeData); if it was greater than nrow(outcomeData), return NA
        num <- if (is.character(num)) {
            switch (tolower(num), best = 1, worst = nrow(x), NA)
        } else {num
        }
        if (all(num > nrow(x))) {return(NA)}
        x[num,1]
    }
    

    # Order the table and split it:
    orderedData <- outcomeData[order(outcomeData[,idx],outcomeData$Hospital.Name),c("Hospital.Name","State")]
    byState <- split(orderedData, orderedData$State)
    temp = sapply(byState, getIndex, num)
    data.frame(hospital = unname(temp), state = names(temp))
    
}