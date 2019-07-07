pollutantmean <- function(directory, pollutant, id = 1:332){
  
    lessThanTen <- id[id < 10]
    if(length(lessThanTen) != 0){
    lessThanTen <- paste("00", lessThanTen, sep = "")
    }
    
    lessThanHundred <- id[id< 100 & id > 9]
    if(length(lessThanHundred) != 0){
    lessThanHundred <- paste("0", lessThanHundred, sep = "")
    }
    
    moreThanHundred <- id[id > 99]
    
    finalID <- c(lessThanTen, lessThanHundred, moreThanHundred)
    means <- vector()
    
    if(pollutant == "sulfate"){
      number <- 2
    }
    else{
      number <- 3
    }
    
    for(i in finalID){
      x <- read.csv(paste(directory,"/",i,".csv",sep = ""))
      means <- c(means, x[[number]])  
    }
    
    mean(means, na.rm = TRUE)
}

