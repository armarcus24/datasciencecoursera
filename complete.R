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
  
  
  ##setwd(directory)
  files <- list.files()
  dataset <- data.frame()
  nobs <- data.frame()
  
  for(i in id) {
    dataset <- rbind(dataset, read.csv(files[i]))
    nobs <- rbind(nobs, c(i, sum(complete.cases(dataset))))
    dataset <- data.frame()
  }
  
  colnames(nobs) <- c("id", "nobs")
  return(nobs)
  
}