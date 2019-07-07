complete <- function(directory, id = 1:332){
  
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
  
  temptable1 <- vector()
  temptable2 <- vector()
  
  for(i in finalID){
    x <- read.csv(paste(directory,"/",i,".csv",sep = ""))
    good <- complete.cases(x)
    woowoo <- x[good, ]
    temptable1 <- c(temptable1, i)
    temptable2 <- c(temptable2,nrow(woowoo))
  }
  finaltable <- data.frame(id = temptable1, nobs = temptable2)
  finaltable

}

