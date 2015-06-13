pollutantmean <- function(directory, pollutant, id=1:332){
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'pollutant' is a character vector of length 1 indicating
    ## the name of the pollutant for which we will calculate the
    ## mean; either "sulfate" or "nitrate".
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return the mean of the pollutant across all monitors list
    ## in the 'id' vector (ignoring NA values)
    ## NOTE: Do not round the result!
    
    ## START:
    
    ## Check Inputs: 
    
    # Check that the directory exists: 
    stopifnot(length(directory)==1,dir.exists(directory))
    
    # Check that the pollutant is a character vector containing exactly one of the allowed names:
    allowedPollutants <- c("sulfate","nitrate");
    stopifnot(is.character(pollutant),length(pollutant)==1,
              grep(pollutant,allowedPollutants,ignore.case=TRUE)>0)
    
    # Check that the ID is an integer list:
    stopifnot(is.integer(id) || (is.numeric(id) && identical(round(id),id)))
    
    # Remove bad values from the ID list: 
    if ( any(id < 1) || any(id > 332) ){
        warning("ID values out of range 1:332 detected and ignored.")
    }
    if ( any(is.na(id)) ) {
        warning("NA ID values detected and ignored.")
    }
    id <- id[id > 0 & id < 333 & !is.na(id)]
    stopifnot(length(id) > 0)
    
    ## Get the data: 
    
    for (n in id){
        fname <- sprintf("%03d",n)
        d = read.csv(file.path(directory,fname))
        
    }
    
}