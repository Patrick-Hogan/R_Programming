corr_vectorized <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    ## NOTE: Do not round the result!
    
    ## START:
    
    ## Check Inputs: 
    
    # Check that the directory exists: 
    stopifnot(length(directory)==1,dir.exists(directory))
    allFiles = dir(directory)
    
    # Check that the threshold is a scalar number:
    stopifnot(is.numeric(threshold) && length(threshold)==1)

    ## Get the data
    # Allocate the correlation vector: 
#     correlation = vector("numeric",length(allFiles))
# 
#     for (n in 1:length(allFiles)) { 
#         temp <- read.csv(file.path(directory,allFiles[n]))
#         good <- complete.cases(temp)
#         correlation[n] <- if (sum(good) > threshold){
#             cor(temp[good,"sulfate"],temp[good,"nitrate"])
#         } else { NA
#             # d = rbind(d,temp[good,])
#         } 
#     }
    
    # This is equivalent to the above code, but should be more efficient (maybe)
    allData = lapply(file.path(directory,allFiles),read.csv)
    sub_corr = function(d,threshold){
        good = complete.cases(d)
        if (sum(good) > threshold){cor(d[good,"sulfate"],d[good,"nitrate"])
        } else {NA}
    }
    # Use "sapply" to return an array rather than list; "threshold" is passed 
    # through to sub_corr: 
    correlation = sapply(allData,sub_corr,threshold)

    # Remove the NA's from the array:    
    correlation <- correlation[!is.na(correlation)]
}