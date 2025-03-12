# Data from Oliver Binks
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork) # to plot several plots as panels

rm(list = ls())

path <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
subpath <- "data/"
source(paste0(path,"common_functions.R"))
# load file
filename_CB <- "CB dendro data.csv"
filename_RC <- "RC dendro data.csv"
cb_raw <- read.csv(paste0(path, subpath, filename_CB))
rc_raw <- read.csv(paste0(path, subpath, filename_RC))

transform_dendrometer_data <- function(inDF) {
  # extract tree names
  trees <- colnames(inDF)[2:(length(inDF))]
  # create a date vector
  inDF$Date <- as.Date(inDF$DT)
  # create new df
  newDF <- data.frame(matrix(nrow=0, ncol=3))
  colnames(newDF) <- c("Date", "Tree", "Circum_mm")
  # go through the columns (trees) and bind them as rows one above the other
  for (tree in trees){
    # for this <tree>, these are the variables:
    Date <- inDF$Date
    Circum <- inDF[[tree]]
    thistree <- data.frame(Date=Date, Tree=tree, Circum_mm=Circum)
    # bind the trees one above the other
    newDF <- rbind(newDF, thistree)
  }
  # Add month and year columns
  newDF$month <- month(newDF$Date)
  newDF$year <- year(newDF$Date)
  # calculate diameter from circumference + convert to cm from mm
  newDF$DBH_cm <- newDF$Circum_mm / pi / 10 
  newDF$BA_cm2 <- newDF$DBH_cm ^ 2 / 4 * pi
  # change the order of the variables
  newDF <- newDF[,c(1,4,5,2,3,6,7)]
  # output:
  return(newDF)
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


cd_trasnformed <- transform_dendrometer_data(cb_raw)
rc_trasnformed <- transform_dendrometer_data(rc_raw)

head(rc_trasnformed)
# remove NAs
cd_trasnformed <- cd_trasnformed[!is.na(cd_trasnformed$Circum),]
rc_trasnformed <- rc_trasnformed[!is.na(rc_trasnformed$Circum),]

# Summarise (aggregate) to montly values by taking the 90th percentile value for all the 
# montly data (also ignore NAs) 
fun <- function(x){return(quantile(x, prob=.10))}
# find 90th percentile of daily values
cb_transformed_byday <- aggregate(cd_trasnformed[,c("Circum_mm", "DBH_cm", "BA_cm2")], 
                             by=with(cd_trasnformed, 
                                     list(Date=Date, 
                                          Tree=Tree, 
                                          year=year, 
                                          month=month)), FUN=fun)

rc_transformed_byday <- aggregate(rc_trasnformed[,c("Circum_mm", "DBH_cm", "BA_cm2")], 
                                  by=with(rc_trasnformed, 
                                          list(Date=Date, 
                                               Tree=Tree, 
                                               year=year, 
                                               month=month)), FUN=fun)
# # Test the new DF
# head(cb_transformed_byday)
# ggplot(data=cb_transformed_byday)+
#   geom_line(aes(x=Date, y=DBH_cm))+
#   facet_wrap(.~Tree, scales = "free_y")+
#   theme_bw()

# calculate BA increment (cm2/day) for all the trees
cb_byday <- find_BA_increment_OliverBinks(cb_transformed_byday)
rc_byday <- find_BA_increment_OliverBinks(rc_transformed_byday)
# # Test the new DF
head(rc_byday)
ggplot(data=rc_byday)+
  geom_line(aes(x=Date, y=BA_inc_cm2))+
  facet_wrap(.~Tree, scales = "free_y")+
  theme_bw()

# find monthly value (max?) for increment
# Summarise (aggregate) to montly values by taking the 90th percentile value for all the 
# montly data (also ignore NAs) 
       # fun <- function(x){return(quantile(x, prob=.90, na.rm=T))}
# find 90th percentile of daily values
cb_pertree_bymonth <- aggregate(cb_byday[,c("BA_inc_cm2")], 
                             by=with(cb_byday, 
                                     list(Tree=Tree, month=month)), 
                             FUN=median, na.rm=T)
rc_pertree_bymonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                                by=with(rc_byday, 
                                        list(Tree=Tree, month=month)), 
                                FUN=median, na.rm=T)

ggplot(data=cb_pertree_bymonth)+
  geom_boxplot(aes(x=as.factor(month), y=x), outlier.shape = NA)+
  ylim(-0.1,0.2)
ggplot(data=rc_pertree_bymonth)+
  geom_boxplot(aes(x=as.factor(month), y=x), outlier.shape = NA)+
  ylim(-0.1,0.2)


cb_pertree_bymonth_sitecol <- cb_pertree_bymonth
rc_pertree_bymonth_sitecol <- rc_pertree_bymonth
cb_pertree_bymonth_sitecol$site <- "Cow Bay"
rc_pertree_bymonth_sitecol$site <- "Robson Creek"
colnames(cb_pertree_bymonth_sitecol)[3] <- "BA_inc_cm2_day"
colnames(rc_pertree_bymonth_sitecol)[3] <- "BA_inc_cm2_day"

OB_combined_bymonth <- rbind(cb_pertree_bymonth_sitecol, 
                             rc_pertree_bymonth_sitecol)

ggplot(data=OB_combined_bymonth)+
  geom_boxplot(aes(x=as.factor(month), y=BA_inc_cm2_day, fill=site), outlier.shape = NA)+
  ylim(-0.03,0.1)+
  theme_bw()+
  labs(x="Month", y="BA Increment, cm^2/day")

ggplot(data=OB_combined_bymonth)+
  geom_boxplot(aes(x=as.factor(month), y=BA_inc_cm2_day, fill=site), outlier.shape = NA)+
  # ylim(0,0.3)+
  theme_bw()+
  labs(x="Month", y="BA Increment, cm^2/day")+
  facet_wrap(.~site, ncol=1)

ggplot(data=rc_pertree_bymonth_sitecol)+
  geom_boxplot(aes(x=as.factor(month), y=BA_inc_cm2_day), fill="green2")+
  ylim(0,0.3)+
  theme_bw()+
  labs(x="Month", y="BA Increment, cm^2/day")+
  facet_wrap(.~site, ncol=1)+
  ylim(-0.05, 0.13)+
  ggtitle("Robson Creek point dendrometer data")


cb_bymonth <- aggregate(cb_byday[,c("BA_inc_cm2")], 
                        by=with(cb_byday, 
                                list(month=month)), 
                        FUN=median, na.rm=T)
rc_bymonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                        by=with(rc_byday, 
                                list(month=month)), 
                        FUN=median, na.rm=T)

# just for averages per year
cb_pertree_byyear <- aggregate(cb_byday[,c("BA_inc_cm2")], 
                                by=with(cb_byday, 
                                        list(year=year)), 
                                FUN=mean, na.rm=T)
# just for averages per year
rc_pertree_byyear <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                               by=with(rc_byday, 
                                       list(year=year)), 
                               FUN=mean, na.rm=T)


rc_pertree_bymonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                                by=with(rc_byday, 
                                        list(Tree=Tree, month=month)), 
                                FUN=median, na.rm=T)

