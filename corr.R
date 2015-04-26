scorr <- function(directory, threshold = 0) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'threshold' is a numeric vector of length 1 indicating the
  ## number of completely observed observations (on all
  ## variables) required to compute the correlation between
  ## nitrate and sulfate; the default is 0
  
  ## Return a numeric vector of correlations
  
  files <- list.files(directory)
  dataset <- data.frame()
  nobs <- numeric()
  corrs <- numeric()
  sub <- matrix()
  
  for(i in seq_along(files)) {
    file <- formatC(i, width = 3, format="d", flag = "0")
    filename <- paste(directory, "/", paste(file, ".csv", sep=""), sep="")
    dataset <- rbind(dataset, read.csv(filename))
    nobs <- sum(complete.cases(dataset))
    log <- nobs > threshold
    if (log == "TRUE") {
      sub <- dataset[complete.cases(dataset), ]
      corr <- cor(sub$nitrate, sub$sulfate)
      corrs <- append(corrs, corr)
      dataset <- data.frame()
      next
    }
    else {
      dataset <- data.frame()
      next
    }
  }
  
  return(corrs)
  
}