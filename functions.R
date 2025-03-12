# adds a coulnd of month, day of month and season for an LPJ-GUESS output file.
# Use only with an output filr where each row is one day (nothe that it would not work
# for patch-level outpus)
add_daysmonthsseasons <- function(tempdf) {
  numberofyears <- dim(tempdf)[1]/365
  
  ndaysinmonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  season <- c("summer", "summer", "autumn", "autumn", "autumn", "winter", "winter", "winter",
              "spring", "spring", "spring", "summer")
  
  newdf <- data.frame(matrix(nrow = 365, ncol = 3))
  names(newdf) <- c("dayofmonth", "month", "season")
  
  countdays <- 1
  for (monthnumber in 1:length(ndaysinmonth)) {
    daysthismonth <- ndaysinmonth[monthnumber]
    rangestart <- countdays
    rangeend <- rangestart + daysthismonth - 1
    countdays <- rangeend + 1
    newdf$month[rangestart:rangeend] <- monthnumber 
    newdf$dayofmonth[rangestart:rangeend] <- 1:daysthismonth
    newdf$season[rangestart:rangeend] <- season[monthnumber]
  }
  
  # replicate newdf numberofyears times 
  timecoloumns <- do.call("rbind", replicate(numberofyears, newdf, simplify = FALSE))
  
  tempdf <- cbind(tempdf, timecoloumns)
  
return(tempdf)
}

#####

# a better version of the same function. adds month and season (and in the future day of the month)
# to LPJ-GUESS output dataframe
add_daysmonthsseasonsV2 <- function(tempdf) {
  # add the empty columns
  tempdf$month <- NA
  tempdf$season <- NA
  # NOTE : ADD DAY OF MONTH
  # tempdf$dayofmonth <- NA
  
  ndaysinmonth <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  season <- c("summer", "summer", "autumn", "autumn", "autumn", "winter", "winter", "winter",
              "spring", "spring", "spring", "summer")
  
  # days start in 0 (C++)
  countdays <- 0
  for (monthnumber in 1:length(ndaysinmonth)) {
    daysthismonth <- ndaysinmonth[monthnumber]
    rangestart <- countdays
    rangeend <- rangestart + daysthismonth - 1
    # add the month number for this month to the correct location across the dataframe
    tempdf[(tempdf$Day >= rangestart) & (tempdf$Day <= rangeend),]$month <- monthnumber
    # add the season number for this month to the correct location across the dataframe
    tempdf[(tempdf$Day >= rangestart) & (tempdf$Day <= rangeend),]$season <- season[monthnumber]
    # increase the day counter to be the start of the next range of dates
    countdays <- rangeend + 1
  }
  
  return(tempdf)
}

#####

# 
# prepare_plotting_variable_perpft <- function(variable_to_plot, 
#                                              basepath, site, version) {
#   
#   out <- list()
#   # go through variables to plot
#   for (var in variable_to_plot){
#     // read the 
#     tempdf <- read.table(paste0(basepath,"/",site,"/",version,"/",var), header=T)
#     tempdf <- add_daysmonthsseasons(tempdf)
#     
#     if (var == variable_to_plot[1]) {
#       # save the first file name
#       vars_to_plot <- var
#       # save date vector
#       date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
#       season <- tempdf$season
#       month <- tempdf$month
#       # set length of first file to be the "standard" for analysis
#       datalength <- dim(tempdf)[1]
#       out[[var]] <- tempdf
#     }
#     # is there a pft column?
#     ispft <- length(which(colnames(tempdf) == pft)) != 0
#     # is there the same length as the first variable?
#     issamelength <- dim(tempdf)[1] == datalength
#     # create output file with only variables that have the same pft and have the same length
#     if (issamelength && ispft && var != variable_to_plot[1]) {
#       out[[var]] <- tempdf
#       # add the file name to the vector of file names to be plotted
#       vars_to_plot <- c(vars_to_plot, var)
#     } 
#     # save date vector
#     if (var == variable_to_plot[1]) {
#       date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
#     }
#     
#   }
#   
#   return(out)
# }
# 
