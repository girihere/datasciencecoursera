corr <- function(directory, threshold = 0){
  
  id = 1:332
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
  overallcount <- 0
  
  for(i in finalID){
    x <- read.csv(paste(directory,"/",i,".csv",sep = ""))
    good <- complete.cases(x)
    woowoo <- x[good, ]
    
    if(overallcount + nrow(woowoo) >= threshold){
      temptable1 <- c(temptable1, woowoo[1:(threshold - overallcount),2])
      temptable2 <- c(temptable2, woowoo[1:(threshold - overallcount),3])
      break;
    } 
    
    else{
    temptable1 <- c(temptable1, woowoo[2])
    temptable2 <- c(temptable2, woowoo[3])
    overallcount <- overallcount + nrow(woowoo)
    }
    
    print(overallcount)
  }
  
  
  temptable1 <- as.numeric(unlist(temptable1))
  temptable2 <- as.numeric(unlist(temptable2))
  cor(temptable1, temptable2)
}


