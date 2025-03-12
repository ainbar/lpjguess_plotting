# !!!!!!!!!!!!!!!!THIS IS NOT COMPLETE
# TO DO
# IN THE LOOP BELOW, READ EACH FILE INTO MEMORY AND AGGREGATE (USING THE 0
# AGGREGATE FUNTION) INTO DATES (SUM) THE SOLAR RADIATION. BECASUE DATES WILL BE 
# SPLIT IN HALF (THE FILES ARE SPLIT IN THE MIDDLE OF DATES), MEANS THAT ANOTHER 
# AGGREGATION NEEDS TO BE DONE ACCORDING TO DATE

# GOT THE ANALISED LAI FILE FROM BELINDA (THE ONE CALCULATE FROM THE PAR SE$NSORS)
# SO CONTINUE WORKING ON THE CODE HWER ONLY IF NECESSARY.

rm(list=ls())
library(simtimer)

# read in the files
directoryname <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/data/CUP_radiation/"

# crete a new dataframe that will hold all the data
data <- read.csv(paste0(directoryname, "CUP_AUTO_radiation_20180930.dat"))
data_temp <- read.csv(paste0(directoryname, "CUP_AUTO_radiation_20181031.dat"))
newdf <- data.frame(matrix(ncol=3, nrow=0))
colnames(newdf) <- c("timestamp", "record.num", "downwelling.avg")
# get the naes of all the files that will go into that new dataframe
filelist <- list.files(directoryname, pattern = "CUP_AUTO_radiation")


for (filename in filelist) {
  # read in file into dataframe
  data_from_file <- read.csv(paste0(directoryname, filename))
  # remove first 3 lines
  data_from_file <- data_from_file[-(1:3),]
  colnames(data_from_file)[1] <- "timestamp"
  
  data_from_file$timestamp <- as.POSIXct(data_from_file$timestamp,tz=Sys.timezone())
  
  thisfiledf <- data_from_file[1:dim(data_from_file)[1], 1:3]
  
  colnames(thisfiledf) <- c("timestamp", "record.num", "downwelling.avg")
  head(thisfiledf)
  
  # checkif there are NAs and report back
  if (sum(is.na(thisfiledf$downwelling.avg)) > 0) {
    print(paste0("There are NAs in the file: ", filename))
  }
  # aggregate minute to daily
  thisfiledf_daily <- 
  # add contents at the end of the main  dataframe
  newdf <- rbind(newdf, thisfiledf_daily)

}
