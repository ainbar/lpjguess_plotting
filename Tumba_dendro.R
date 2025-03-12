# Site: Tumbarumba
# period: 2001-2002
# source: Heather Keith's hard drive (Laura 03/06/2024)
# paper: Keith et al 2012
# '...The open, wet sclerophyll forest is predominantly E. delegatensis (R.T. Baker) 
# (Alpine Ash) in pure and mixed stands with E. dalrympleana (Maiden) (Mountain Gum) 
# and E. stellulata (Sieber ex DC) (Black Sallee), with a  ulti-aged structure 
# and an average dominant height of 40 m. The understorey is heterogeneous with 
# patches of grasses, low and tall shrubs..."
# AA - alpine ash
# MS - Mountain gum
# BS - Black Sallee

library(ggplot2)
library(doBy)
library(dplyr)
library(tidyverse)
library(patchwork) # to plot several plots as panels
library(readxl)

rm(list = ls())

filename_tumba <- "data/Dendrometer_trees_tumba.xls"
folder <- "~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"

setwd(folder)
# read in excel file

convert_file_to_raw_data_tumba <- function(inDF){
  outDF <- inDF[,3:dim(inDF)[2]]
  outDF_colnames <- paste0(as.character(outDF[3,]), as.character(outDF[4,]))
  colnames(outDF) <- outDF_colnames
  outDF <- outDF[6:dim(outDF)[1], ]
  outDF <- outDF[1:(dim(outDF)[1]-3), ]
  # remove the distance from dendro column
  outDF <- outDF[,-2]
  # change the column nam,es to these
  colnames(outDF) <- c("Tree","height_m", "Low_crownheight_m", "Species",
                           "InitialBA_cm2", "26.09.01", "17.10.01", "14.11.01",
                           "12.12.01", "16.01.02", "13.02.02", "13.03.02", 
                           "10.04.02","8.05.02","4.06.02", "10.7.02","22.8.02",
                           "18.9.02", "16.10.02", "12.11.02", "18.12.02", "15.1.03",
                           "12.2.03", "12.3.03", "9.4.03", "8.5.03", "12.6.03",
                           "10.7.03", "4.9.03", "2.10.03", "14.10.03", "30.10.03", 
                           "25.11.03")
  
  return(outDF)
}

reshape_raw_tumba <- function(raw_tumba) {
  dates <- colnames(raw_tumba)[6:length(colnames(raw_tumba))]
  dates <- as.Date(dates, format="%d.%m.%y")
  numberofdates <- length(dates)
  
  treenumbers <- as.numeric(raw_tumba$Tree)
  
  outDF <- data.frame(matrix(nrow=0,ncol=4))
  colnames(outDF) <- c("Date", "Tree","BA_cm2", "species")
  
  for (tree in treenumbers) {
    thistree <- data.frame(Date=dates, 
                           Tree=tree, 
                           BA_cm2=as.numeric(raw_tumba[tree,6:dim(raw_tumba)[2]]),
                           species=raw_tumba$Species[tree])
    outDF <- rbind(outDF, thistree)
    
  }
  
  return(outDF)
}

# calculate daily BA increment
find_BA_increment_OliverBinks <- function(inDF){
  # create an empty column
  inDF$BA_inc_cm2 <- NA
  # create an empty DF for outputs
  outDF <- inDF[inDF$Tree == "asdasdas", ]
  trees <- unique(inDF$Tree)
  for (tree in trees) {
    thistree <- inDF[inDF$Tree == tree, ]
    head(thistree)
    # make sure that the new df is ordered by Date
    thistree <- thistree[order(thistree$Date),]
    # only continue when there are no duplicate dates
    if (!sum(duplicated(thistree$Date))) {
      for (t in 2: dim(thistree)[1]) {
        # go through the dates in each treeID, and calculate the tree BA increment 
        thisdate     <- thistree$Date[t]
        previousdate <- thistree$Date[t-1]
        daysbetween  <- as.numeric(thisdate - previousdate)
        thistree$BA_inc_cm2[t] <- (thistree$BA_cm2[t] - thistree$BA_cm2[t-1]) / daysbetween
      }
      # bind the trees into one dataframe
      outDF <- rbind(outDF, thistree)
    } else {
      print(paste0("tree ", tree, " has duplicate dates"))
    }
    # go through the dates and calculate increments
  }
  return(outDF)
}

##=======================

# read in Tumb file
raw_tumba_fromfile <- read_excel(paste0(folder, filename_tumba), sheet = "Basal area (cm2) 30 sites")
# prepare the tumba file to work with
raw_tumba <- convert_file_to_raw_data_tumba(raw_tumba_fromfile)
rm(raw_tumba_fromfile)
# this dataframe can be used later for different things
initial_tree_measurements <- raw_tumba[,1:5]
# write_csv(initial_tree_measurements, "data/Tumba_JEndland_initial_dendro_tree_measurements.csv")

##=======================

# reshape the raw data by adding the trees and their measurements in rows
tumba <- reshape_raw_tumba(raw_tumba)
# remove NAs
tumba <- tumba[!is.na(tumba$BA_cm2), ]

# - Calculate BA increment per tree.
# - make sure that the dates are in the right order prior.
# - divide by the number of days between measruemtns to get BA in units of cm2/day
# calculate BA increment for each tree
tumba <- find_BA_increment_OliverBinks(tumba)
# add month and year columns
tumba$month <- month(tumba$Date)
tumba$year  <- year(tumba$Date)

# calculate relative diameter incremnt 
tumba$BA_inc_rel <- tumba$BA_inc_cm2 / tumba$BA_cm2
head(tumba)
##=======================
# 

tumba_bymonth <- aggregate(tumba[,c("BA_inc_cm2", "BA_inc_rel")],
                                  by=with(tumba,
                                          list(Tree=Tree,
                                               species=species,
                                               year=year,
                                               month=month)), FUN=median)


tumba_bymonth_sumBAinc <- aggregate(tumba[,"BA_inc_cm2"],
                           by=with(tumba,
                                   list(Tree=Tree,
                                        species=species,
                                        year=year,
                                        month=month)), FUN=sum)
colnames(tumba_bymonth_sumBAinc)[5] <- "BA_inc_cm2"

ggplot(data=tumba_bymonth, aes(x=as.factor(month), y=BA_inc_rel*100))+
  geom_boxplot(outlier.shape = NA, fill="green")+
  geom_jitter(color="black", size=0.4, alpha=0.1)+
  theme_bw()+
  ylim(-0.005, 0.008)+
  labs(x="month", y="relative daily BA growh inc (% of BA)")

# absoluite sum of the monthly daily increments
ggplot(data=tumba_bymonth_sumBAinc, aes(x=as.factor(month), y=BA_inc_cm2))+
  geom_boxplot(outlier.shape = NA, fill="green")+
  theme_bw()+
  labs(x="month", y="total stand BA growh inc, cm2/month")+
  ylim(-0.03, 0.03)

ggplot(data=tumba_bymonth_sumBAinc)+
  geom_boxplot(aes(x=as.factor(month), y=BA_inc_cm2, fill=as.factor(month)),
               outlier.shape = NA)+
  # geom_jitter(color="black", size=0.4, alpha=0.1)+
  theme_bw()+
  labs(x="month", y="total stand BA growh inc, cm2/month")+
  facet_wrap(.~year)+
  ylim(-0.05, 0.1)

