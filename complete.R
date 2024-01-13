complete <- function(directory, id){
  
  #initalize variables
  FileDirect <- as.character(directory)
  f <- 1
  g <- 1
  Tabme <- data.frame()
  MonitorIDs <- c(id)
  SizeOfFinalVectors <- length(MonitorIDs)
  SumHolder <- numeric()
  SumOfLength <- 0
  FinalDataFrame <- data.frame(
    Entry = numeric(SizeOfFinalVectors),
    IDHolder = numeric(SizeOfFinalVectors), 
    SumNumber = numeric(SizeOfFinalVectors)
  )
  
  #set directory based off user input
  wd1 <- c("/Users/joshuastein/Desktop/hello-world/Airpollutionproject/")
  wd2 <- paste(wd1, FileDirect, sep = "")
  setwd(wd2)
  
  #go through the folder and search for the number of IDs
  for (i in MonitorIDs){
    SumOfLength <- 0
    
    #create file name based off i and extract data
    FileNum <- sprintf("%03d.csv", i)
    Tabme <- read.csv(FileNum)
    
    # take length of values in data frame that aren't NA
    SumOfLength <- length(Tabme$sulfate[!is.na(Tabme$sulfate)])
    SumHolder[f] <- c(SumOfLength)
    f <- f + 1 
  }
   
  #Prep entry into FinalDataFrame
  RevSumHolder <- rev(SumHolder)
  Entries <- c(1:SizeOfFinalVectors)
  
  # insert data into FinalDataFrame
  FinalDataFrame$Entry <- Entries
  FinalDataFrame$IDHolder <- MonitorIDs
  FinalDataFrame$SumNumber <- SumHolder
  
  # Print
  print(FinalDataFrame)
}

#Example Usage
complete("specdata", c(2, 4, 8, 10, 12))
complete("specdata", 30:25)
complete("specdata", 3)