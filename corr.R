corr <- function(directory, threshold = 0){
  
  #initalize variables
  FileDirect <- as.character(directory)
  ThresholdVector <- as.numeric(threshold)
  i <- 1
  FinalCorrelations <- numeric()
  
  #set directory based off user input
  wd1 <- c("/Users/joshuastein/Desktop/hello-world/Airpollutionproject/")
  wd2 <- paste(wd1, FileDirect, sep = "")
  setwd(wd2)
  
  #Define NumberOfFiles paramter required for for loop
  AllFiles <- list.files(wd2)
  NumberOfFiles <- length(AllFiles)

  #loop through all files in specdata and save to data frame
  while (i < NumberOfFiles){
    
    #create file name based off i and extract data
    FileNum <- sprintf("%03d.csv", i)
    Tabme <- read.csv(FileNum)
    Tabme <- Tabme[complete.cases(Tabme),]
    
    #Complete threshold comparison, then store in numeric vector
    TotalObservances <- length(Tabme$nitrate)
    if (TotalObservances > ThresholdVector){
      
      #Determine correlations and add to final numeric vector
      WorkingCorrelations <- cor(Tabme$sulfate, Tabme$nitrate, use="pairwise.complete.obs")
      FinalCorrelations <- c(FinalCorrelations, WorkingCorrelations)
    }
    
    #increment i 
    i <- i + 1
  }
  #return data
  return(FinalCorrelations)
}
  #example usage
cr <- corr("specdata", 150)
head(cr)
summary(cr)
cr <- corr("specdata", 400)
head(cr)
summary(cr)
cr <- corr("specdata", 5000)
summary(cr)
length(cr)
cr <- corr("specdata")
summary(cr)
length(cr)

