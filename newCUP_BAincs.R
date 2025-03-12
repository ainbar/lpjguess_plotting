library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(doBy)
library(patchwork) # to plot several plots as panels

# rm(list = ls())

folder_DE_dendro <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/EucFACE/DE_dendro/'

# read in file (list type)
raw2012_2013_list <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_CORR_FEB2012-DEC2013_v1.csv'), header = TRUE)
raw2014_2015_list <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_CORR_MAR2014-DEC2015_v1.csv'), header = TRUE)
raw2016_2017_list <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_CORR_MAR2016-DEC2017_v1.csv'), header = TRUE)
raw2018_2019_list <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_CORR_JAN2018-DEC2019_v2.csv'), header = TRUE)
raw2020_2021_list <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_CORR_JAN2020-DEC2021_v1.csv'), header = TRUE)

# read in file (table type)
raw2013_2014_table <- read.csv(paste0(folder_DE_dendro,'FACE_P0025_RA_TREEMEAS_2013-14_RAW_V1.csv'), header = TRUE)
# remove the row called "CORR3" from the file
raw2013_2014_table <- raw2013_2014_table[, c(colnames(raw2013_2014_table)[1:12], colnames(raw2013_2014_table)[14:18])]

# read file with more tree info (canopy status: suppressed, co-dominant, dominant, dead, alive)
# file sent by DE (17/04/2024)
Tree_classifications <- read.csv(paste0(folder_DE_dendro,'Tree_classifications.csv'), header = TRUE)
# how many trees per classification
Tree_class_summary <- summaryBy(Tree ~ Classif, data=Tree_classifications, FUN=length)
# Classif Tree.length
# 1 Codominant          67
# 2   Dominant          31
# 3 Suppressed          62

# Suppressed trees needs to be removed (later)

######### ######### ######### 
######### FUNCTIONS ######### 
######### ######### ######### 

# "table"-type data frames needs to be converted to list-type so the y could be bound together.
reshapeTable <- function(tempDF) {
  # names of columns with dates
  dates <- colnames(tempDF[4:length(colnames(tempDF))])
  # add the column "Tree" to the names
  selectedcols <- c( colnames(tempDF[2]), dates )
  # select only the columns with the same names from the list
  tempDF <- tempDF[,selectedcols]
  # reshape the table
  tempDFreshaped <- melt(tempDF,id.vars=c("Tree"))
  colnames(tempDFreshaped) <- c("Tree", "Date", "DBH_corr")
  # remove the "X" from the Date field
  tempDFreshaped$Date <- gsub("X","",as.character(tempDFreshaped$Date))
  # replace the "." with "/" in the date field
  tempDFreshaped$Date <- gsub("\\.","/",as.character(tempDFreshaped$Date))
  # add "empty (NA) Ring" and "CO2.trt" columns
  tempDFreshaped$Ring <- NA
  tempDFreshaped$CO2.trt <- NA
  # change the column numbers to fit the "list" dataframes
  # tempDFreshaped <- tempDFreshaped
  tempDFreshaped <- tempDFreshaped[,c(1,4,5,2,3)]
  
  return(tempDFreshaped)
}

# assign ring number and CO2 treatments to every row win the new "list"-type dataframe
assign_ring_and_treatment <- function(tree_ring_CO2_df, inDF) {
  # run through the lines in the new "list"-type dataframe
  for (r in 1:dim(inDF)[1]) {
    # the current tree number
    current_Tree <- inDF$Tree[r]
    # the row index for the current tree
    rowindex <- which(grepl(current_Tree, tree_ring_CO2_df$Tree))
    # what ring is the current tree in
    inDF$Ring[r] <- tree_ring_CO2_df[rowindex,][2]$Ring
    # what treatment is it
    inDF$CO2.trt[r] <- tree_ring_CO2_df[rowindex,][3]$CO2.trt
    
  }
  return(inDF)
}

one_value_per_month <- function(inDF) {
  # makes sure there is only one value per month for a specific tree (for monthly 
  # calculations). In case of more than one measurements were taken within one 
  # month (for the same year and the same tree), take the maximum value as the 
  # representative of the month
  
  # vector to save the rows that needs to be removed from the input DF
  to_remove <- 0
  # loop through the length of the DF
  for (i in 2:length(inDF$Tree)) {
    this_tree <-inDF$Tree[i]
    previous_tree <-inDF$Tree[i-1]
    this_month <- month(inDF$Date[i])
    this_year <- year(inDF$Date[i])
    previous_month <- month(inDF$Date[i-1])
    previous_year <- year(inDF$Date[i-1])
    same_tree <- this_tree == previous_tree
    same_month <- this_month == previous_month
    same_year <- this_year == previous_year
    if ( same_tree && same_month && same_year ) {
      # this means that there is more than one measurement per month for the same 
      # Tree. This will keep only the row with the maximum value (per month, 
      # year, tree) and remove the rest (by saving it in the *to_remove* vector)
      this_dbh <- inDF$DBH_corr[i]
      previous_dbh <- inDF$DBH_corr[i-1]
      
      if ( this_dbh < previous_dbh ) {
        # in case the tree diameter is lower than the previous one at i-1
        # change the tree diameter[i] to be the larger on (at i-1). 
        # The row below (i) is then redundant (will be saved for deletion after 
        # the if)
        inDF$DBH_corr[i] <- previous_dbh # this_dbh <- previous_dbh
      } 
      # save the row number that needs to be removed later
      to_remove <- c(to_remove, i-1)
    }
  }
  
  # remove the duplicates (this should keep the maximum DBH measured for the same 
  # month and tree)
  return(inDF[-to_remove, ])
}

# calculates the increments from 
calulate_increments <- function(inDF){
  inDF$DBH_inc <- NA
  inDF$BA_inc <- NA
  inDF$cumul_BA_inc <- 0
  inDF$cumul_DBH_inc <- 0
  # run through every row in the inout df
  for (i in 2:length(inDF$Tree)) {
    # determone conditions
    this_tree <- inDF$Tree[i]
    previous_tree <- inDF$Tree[i-1]
    # is it the same tree as the previous row?
    same_tree <- this_tree == previous_tree
    # is the date progressing compared to the previous row?
    progressing_date <- inDF$Date[i] > inDF$Date[i-1]
    if ( same_tree && progressing_date ) {
      # is it the same tree and the date is progressig?
      # calcualte the increment in DBH and basal area
      intervaldays <- as.numeric(inDF$Date[i] - inDF$Date[i-1])
      # DBH increment in cm/day
      inDF$DBH_inc[i] <- (inDF$DBH_corr[i] - inDF$DBH_corr[i-1]) / intervaldays
      # BA increment in cm2/day
      inDF$BA_inc[i] <- (inDF$BA[i] -  inDF$BA[i-1]) / intervaldays
      inDF$cumul_BA_inc[i] <- inDF$BA_inc[i] + inDF$cumul_BA_inc[i-1]
      inDF$cumul_DBH_inc[i] <- inDF$DBH_inc[i] + inDF$cumul_DBH_inc[i-1]
    } else if ( same_tree && !progressing_date ) {
      # give error 
      print(paste0("date in time i < or = date in time i-1 in step ", i))
      break
    } 
    
  }
  return(inDF)
}

calculate_increment_V2 <- function(inDF){
  library(dplyr)
  # calculates DBH and BA increments #
  # the difference here is that it doesn't assume that the input
  # dataframe is orginised according to trees and then in increasing dates.
  # inDF <- raw_diameters_noNA
  treenames <- unique(inDF$Tree)
  numberoftrees <- length(treenames)
  
  inDF$DBH_inc <- NA
  inDF$BA_inc <- NA
  inDF$RGR_day <- NA
  inDF$cumul_BA_inc <- 0
  inDF$cumul_DBH_inc <- 0
  # create an output dataframe
  outDF <- inDF[inDF$Ring == "ASDFASDf",]
  
  for ( thistree in treenames ){
    treedata <- inDF[inDF$Tree == thistree,]
    # orginize in increasing dates
    treedata <- dplyr::arrange(treedata, Date) # needs library(dplyr)
    numberofdates <- length(treedata$Date)
    for ( t in 2:numberofdates ) {
      thisdate <- treedata$Date[t]
      previousdate <- treedata$Date[t-1]
      if ( thisdate > previousdate ){
        # how many days between the two intervals
        intervaldays <- as.numeric(treedata$Date[t] - treedata$Date[t-1])
        # calculate DBH increment in cm/day
        treedata$DBH_inc[t] <- (treedata$DBH_corr[t] -  treedata$DBH_corr[t-1]) / intervaldays
        # calculate BA increment in cm2/day
        treedata$BA_inc[t] <- (treedata$BA[t] -  treedata$BA[t-1]) / intervaldays
        # calculate RGR increments
        treedata$RGR_day[t] <- ((treedata$DBH_corr[t] -  treedata$DBH_corr[t-1]) / treedata$DBH_corr[t-1]) / intervaldays
        # calculate cumulative BA increment
        treedata$cumul_BA_inc[t] <- treedata$BA_inc[t] + treedata$cumul_BA_inc[t-1]
        # calculate cumulative DBH increment
        treedata$cumul_DBH_inc[t] <- treedata$DBH_inc[t] + treedata$cumul_DBH_inc[t-1]
      } else {
        # give error 
        print(paste0("date in time i < or = date in time i-1 in step ", i))
        break
      }
    }
    # bind the dataframes into the output dataframe
    outDF <- rbind(outDF, treedata)
  }
  return(outDF)
}

# outputs season name from an input of month
get_season <- function(input_month) {
  if ( input_month > 11 || input_month < 3 ) {return("Summer")}
  else if ( input_month > 2 && input_month < 6 ) {return("Autumn")} 
  else if ( input_month > 5 && input_month < 9 ) {return("Winter")}
  else {return("Spring")}
}

add_season_column <- function(inDF) {
  # adds a season column to a dataframe using a month (numeric) column
  inDF$Season <- ifelse(inDF$month %in% c(12,1,2),"SUMMER",
                        ifelse (inDF$month %in% c(3,4,5), "AUTUMN",
                                ifelse (inDF$month %in% c(6,7,8), "WINTER", "SPRING")))
  return(inDF)
}

# Remove trees that has negative trend of increment
filter_bad_trees <- function(output_df) {
  
  # Function to check if slope is negative
  is_negative_slope <- function(sub_data) {
    model <- lm(cumul_DBH_inc ~ Date, data = sub_data)
    coef <- coefficients(model)[["Date"]]
    coef < 0 # TRUE = negative slope
  }
  
  # Filter out trees with negative slopes
  trees_with_positive_slope <- by(output_df, output_df$Tree, function(sub_data) {
    !is_negative_slope(sub_data)
  })
  
  # Keep only the positive slope trees
  # indices of trees that will be kept (have positive slope)
  positive_slope_indices <- names(trees_with_positive_slope)[trees_with_positive_slope]
  tree_data_filtered <- output_df[output_df$Tree %in% positive_slope_indices,]
  return(tree_data_filtered)
}

# remove rows with NAs from specific variables
remove_NAs <- function(inDF){
  inDF <- inDF[(!is.na(inDF$DBH_inc)|!is.na(inDF$BA_inc)), ] # remove NA from increments
  inDF <- inDF[!is.na(inDF$month), ] # remove NA from month
  inDF <- inDF[!is.na(inDF$Season), ] # remove NA from month
  return(inDF)
}

calc_monthly_incs <- function(inDF) {
  # inDF <- output_df
  # head(inDF)
  inDF$BA_inc_month <- NA
  inDF$DBH_inc_month <- NA
  inDF$RGR_month <- NA
  for (i in 1:length(inDF$month)) {
    inDF$BA_inc_month[i] <- inDF$BA_inc[i] * daysinthismonth(inDF$month[i], inDF$year[i])
    inDF$DBH_inc_month[i] <- inDF$DBH_inc[i] * daysinthismonth(inDF$month[i], inDF$year[i])
    inDF$RGR_month[i] <- inDF$RGR_day[i] * daysinthismonth(inDF$month[i], inDF$year[i])
  }
  return(inDF)
}

# convert tables to list form
raw2013_2014_list_V1 <- reshapeTable(raw2013_2014_table)
head(raw2013_2014_list_V1)
# assign a ring and a treatment to the new dataframe
tree_ring_CO2_df <- unique(raw2012_2013_list[,1:3])
raw2013_2014_list <- assign_ring_and_treatment(tree_ring_CO2_df, raw2013_2014_list_V1)
head(raw2013_2014_list)

# bind all the "list"-type data frames together
raw_diameters <- rbind(raw2012_2013_list, raw2013_2014_list, raw2014_2015_list, 
                       raw2016_2017_list, raw2018_2019_list, raw2020_2021_list)
raw_diameters$Date <- as.Date(raw_diameters$Date,format = "%d/%m/%Y")
head(raw_diameters)

# find typing mistakes and change to estimated measurements
raw_diameters[raw_diameters$Tree=="313" & raw_diameters$Date=="2019-08-19",]$DBH_corr <- 20.54
raw_diameters[raw_diameters$Tree=="316" & raw_diameters$Date=="2019-08-19",]$DBH_corr <- 25.46
raw_diameters[raw_diameters$Tree=="611" & raw_diameters$Date=="2019-09-16",]$DBH_corr <- 16.58
raw_diameters[raw_diameters$Tree=="614" & raw_diameters$Date=="2014-03-21",]$DBH_corr <- 35.82

# remove the rows where there is no 'Date' value and no DBH data 
raw_diameters_noNA <- raw_diameters[!is.na(raw_diameters$DBH_corr),]
raw_diameters_noNA <- raw_diameters_noNA[!is.na(raw_diameters_noNA$Date),]

# sort according to Tree name and the n date
raw_diameters_noNA <- arrange(raw_diameters_noNA, Tree, Date)
# some files have overlap which causes the dataframe to have duplicates.
# find DUPLICATES in the data and remove
raw_diameters_noNA <- distinct(raw_diameters_noNA, Tree, Date, .keep_all = TRUE)
# add basal area column, cm^2/tree
raw_diameters_noNA$BA <- pi * (raw_diameters_noNA$DBH_corr * 0.5) ^ 2
# add month column
raw_diameters_noNA$month <- month(raw_diameters_noNA$Date)
raw_diameters_noNA$monthname <- format(as.Date(raw_diameters_noNA$Date, format="%d/%m/%Y"),"%b")
raw_diameters_noNA$year <- year(raw_diameters_noNA$Date)
raw_diameters_noNA <- add_season_column(raw_diameters_noNA)

# make sure there is only one value per month for a specific tree (for monthly calculations)
# !!!!! NOTE: this has to come *BEFORE* diameter increments are calculated
raw_diameters_noNA <- one_value_per_month(raw_diameters_noNA)
# find DBH and basal area (BA) increments
output_df <- calculate_increment_V2(raw_diameters_noNA)
# 
output_df <- calc_monthly_incs(output_df)
output_df <- output_df[(!is.na(output_df$DBH_inc)|!is.na(output_df$BA_inc)|!is.na(output_df$RGR_day)),] 
names(output_df)

cup_DBH_BA_inc_byyear <- summaryBy(cbind(DBH_inc, BA_inc, RGR_day, BA_inc_month, DBH_inc_month, RGR_month) ~ month+year,
                                   data=output_df, FUN=median, keep.names=T)

