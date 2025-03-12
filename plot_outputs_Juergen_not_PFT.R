library(plotly)
library(ncdf4)

rm(list = ls())
basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"
site     <- "CumberlandPlain"

setwd(paste0(basepath,'/',site,'/out'))
version <- "out"   ## default is 'out'
pft      <- "total"

variable_to_plot <- c("dave_nee.out", "dave_lai.out")

source("/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/functions.R")


out <- list()
for (var in variable_to_plot){
  tempdf <- read.table(paste0(basepath,"/",site,"/",version,"/",var), header=T)
  tempdf <- add_daysmonthsseasonsV2(tempdf)
  
  if (var == variable_to_plot[1]) {
    # save the first file name
    vars_to_plot <- var
    # save date vector
    date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
    season <- tempdf$season
    month <- tempdf$month
    # set length of first file to be the "standard" for analysis
    datalength <- dim(tempdf)[1]
    out[[var]] <- tempdf
  }
  # is there a pft column?
  ispft <- length(which(colnames(tempdf) == pft)) != 0
  # is there the same length as the first variable?
  issamelength <- dim(tempdf)[1] == datalength
  # create output file with only variables that have the same pft and have the same length
  if (issamelength && ispft && var != variable_to_plot[1]) {
    out[[var]] <- tempdf
    # add the file name to the vector of file names to be plotted
    vars_to_plot <- c(vars_to_plot, var)
  } 
  # save date vector
  if (var == variable_to_plot[1]) {
    date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
  }
  
}

########## ########## ########## 
######## TIMESERIES ########## 
########## ########## ########## 
# plot one by one using plotly
fig_timeseries <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_timeseries <- fig_timeseries  %>% 
    add_lines(x = date, y = out[[var]][,pft], name = var)
}
fig_timeseries 

########## ########## ########## 
########## MONTHLY ########## 
########## ########## ########## 
# plot one by one using plotly
fig_month <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_month <- fig_month  %>% 
    add_boxplot(x = month, y = out[[var]][,pft], name = var)
  # add_lines(x = season, y = out[[var]][,pft], name = var)
}
fig_month
