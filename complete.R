complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    ## START:
    
    ## Check Inputs: 
    
    # Check that the directory exists: 
    stopifnot(length(directory)==1,dir.exists(directory))
    
    # Check that the ID is an integer list:
    stopifnot(is.integer(id) || (is.numeric(id) && identical(round(id),id)))
    id = as.integer(id)
    
    # Remove bad values from the ID list: 
    if ( any(id < 1) || any(id > 332) ){
        warning("ID values out of range 1:332 detected and ignored.")
    }
    if ( any(is.na(id)) ) {
        warning("NA ID values detected and ignored.")
    }
    id <- id[id > 0 & id < 333 & !is.na(id)]
    stopifnot(length(id) > 0)
    
    ## Get the data
    # Allocate the data frame: 
    d = data.frame(id, nobs=vector("numeric",length(id)))
    
    for (n in 1:length(id)) { 
        temp <- read.csv(file.path(directory,sprintf("%03d.csv",id[n])))
        d$nobs[n] <- sum(complete.cases(temp))
    }
    d
}