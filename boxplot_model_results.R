# Data from Oliver Binks
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork) # to plot several plots as panels

rm(list = ls())

get_month_from_day <- function(day_of_year) {
  
  # Create a vector of days in each month
  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  
  # Find the month for the given day_of_year
  cumulative_days <- cumsum(days_in_month)
  month <- which((day_of_year+1) <= cumulative_days)[1]
  
  # # Return the month name
  # return(month.name[month])
  # Return month number
  return(month)
}

add_month_column <- function(var_df) {
  var_df$month <- NA
  for (i in 1:dim(var_df)[1]) {
    var_df$month[i] <- get_month_from_day(var_df$Day[i]) 
  }
  return(var_df)
}


basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"
# site  <- "WombatStateForest"
#site   <- "Tumbarumba"
site   <- "CumberlandPlain"
# site  <- "RobsonCreek"
setwd(paste0(basepath,'/',site,'/out'))

version <- "out"   ## default is 'out'
pft      <- "TeBE"


list.files(paste0(basepath,'/',site,'/out'))
# 
# var <- "dave_dalpha_leaf.out"
# var <- "dave_dalpha_root.out"
# var <- "dave_dalpha_sap.out"
# var <- "dave_dalpha_repr.out"
var <- "dave_gpp.out"
# var <- "dave_indiv_assim_kg.out"   
# var <- "dave_wscal.out" 
# var <- "dave_nee.out" 
var <- "dave_indiv_npp.out"
# var <- "dave_cgrow_leaf.out"
# var <- "dave_cgrow_repr.out"
# var <- "dave_cgrow_root.out"
# var <- "dave_cgrow_sap.out"
# var <- "dave_diameter_inc.out"
# var <- "dave_height_inc.out"

var_df <- read.table(paste0(basepath,'/',site,'/out/',var), header=T)
head(var_df)

# ignore first 100 years of spinup
var_df <- var_df[36501:dim(var_df)[1],]
# add a month column
var_df <- add_month_column(var_df)
head(var_df)

var_df_month_year <- aggregate(var_df[,c("total")], 
                               by=with(var_df, 
                                       list(Year=Year, month=month)), 
                               FUN=sum, na.rm=T)
head(var_df_month_year)

ggplot(data=var_df_month_year)+
  geom_boxplot(aes(x=as.factor(month), y=x), fill="yellow")

