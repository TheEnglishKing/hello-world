pollutantmean <- function(directory, pollutant, userid){

  #initalize variables
  filedirect <- as.character(directory)
  pollutantc <- as.character(pollutant)
  monitorIDs <- c(userid)
  pollutanttotal <- c(0)
  runningtotal <- c(0)
  tabme <- data.frame()
  
  #set directory based off user input
  wd1 <- c("/Users/joshuastein/Desktop/hello-world/Airpollutionproject/")
  wd2 <- paste(wd1, filedirect, sep = "")
  setwd(wd2)

  # go thorugh each monitorID file, calculate mean of either sulfate or
  # nitrate, then store in a data frame
 for (i in monitorIDs){
   
    #extact the file
    filenum <- sprintf("%03d.csv", i)
    tabme <- read.csv(filenum)
    if (pollutantc == "nitrate") {
      pollutanttotal <- mean(tabme$nitrate, na.rm = TRUE)
      runningtotal <- c(runningtotal, pollutanttotal)
    }
    if(pollutantc == "sulfate"){
      pollutanttotal <- mean(tabme$sulfate, na.rm = TRUE)
      runningtotal <- c(runningtotal, pollutanttotal)
    }
 }
  # remove 0 entered when intializing data frame
  runningtotal <- runningtotal[-1]
  
  # take mean
  summe <- mean(runningtotal)
  
  # print mean
  print(summe)
}
# example usage
pollutantmean("specdata", "sulfate", 1:10)
pollutantmean("specdata", "nitrate", 70:72)
pollutantmean("specdata", "nitrate", 23)


