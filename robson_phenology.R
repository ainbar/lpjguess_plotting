# Methods:
# point dendromoeter at 30m minute intervals.
# 1. calculate for each tree, the 10% percentile of daily basal area (BA).
# 2. calculate the daily BA increment for each tree (BA_INC = BA(t)-BA(t-1))
# 3. take the median daily increment for each tree per month. (rc_pertree_bymonth)

# rm(list = ls())

path <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
subpath <- "data/"
source(paste0(path,"common_functions.R"))

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
  # inDF <- rc_transformed_byday # for debugging
  # create an empty column
  inDF$BA_inc_cm2 <- NA
  inDF$RGR_day <- NA
  # create an empty DF for outputs
  outDF <- inDF[inDF$Tree == "asdasdas", ]
  head(outDF)
  # tree names
  trees <- unique(inDF$Tree)
  for (tree in trees) {
    thistree <- inDF[inDF$Tree == tree, ]
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
        thistree$RGR_day[t] <- (thistree$DBH_cm[t] - thistree$DBH_cm[t-1]) / thistree$DBH_cm[t-1] / daysbetween
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

# plots the daily data 
plot_test_daily_data <- function(inDF){
  ggplot(data=rc_byday)+
    geom_line(aes(x=Date, y=BA_inc_cm2))+
    facet_wrap(.~Tree, scales = "free_y")+
    theme_bw()
}

######### ######### ######### 
######### BASAL AREA ######### 
######### ######### ######### 
# load file
filename_RC <- "RC dendro data.csv"
rc_raw <- read.csv(paste0(path, subpath, filename_RC))
# transform data to workable state
rc_trasnformed <- transform_dendrometer_data(rc_raw)
# remove NAs
rc_trasnformed <- rc_trasnformed[!is.na(rc_trasnformed$Circum),]
head(rc_trasnformed)


# find 10th percentile of daily values
fun <- function(x){return(quantile(x, prob=.10))}
# Summarise (aggregate) to daily values by taking the 10th percentile value
rc_transformed_byday <- aggregate(rc_trasnformed[,c("Circum_mm", "DBH_cm", "BA_cm2")], 
                                  by=with(rc_trasnformed, 
                                          list(Date=Date, 
                                               Tree=Tree, 
                                               year=year, 
                                               month=month)), FUN=fun)
# # Test
head(rc_transformed_byday)
ggplot(data=rc_transformed_byday)+
  geom_line(aes(x=Date, y=DBH_cm))+
  facet_wrap(.~Tree, scales = "free_y")

rc_transformed_bymonth <- aggregate(rc_transformed_byday[,c("Circum_mm", "DBH_cm", "BA_cm2")], 
                                  by=with(rc_transformed_byday, 
                                          list(Tree=Tree, 
                                               year=year, 
                                               month=month)), FUN=fun)

head(rc_transformed_bymonth)
# calculate BA increment (cm2/day) for all the trees
rc_byday <- find_BA_increment_OliverBinks(rc_transformed_byday)

# # Test the new DF
head(rc_byday)
# plot to test
plot_test_daily_data(rc_byday)

# for each tree, take the median daily increment for every month
rc_pertree_bymonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                                by=with(rc_byday, 
                                        list(Tree=Tree, month=month)), 
                                FUN=median, na.rm=T)
head(rc_pertree_bymonth)

# for each tree, take the median daily increment for every  month and every year
rc_pertree_peryear_permonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                                by=with(rc_byday, 
                                        list(month=month, year=year,Tree=Tree)), 
                                FUN=median, na.rm=T)
colnames(rc_pertree_peryear_permonth)[4] <- "BA_inc_cm2_day"
head(rc_pertree_peryear_permonth)

ggplot(data=rc_pertree_bymonth)+
  geom_boxplot(aes(x=as.factor(month), y=x), outlier.shape = NA)+
  ylim(-0.1,0.2)
colnames(rc_pertree_bymonth)[3] <- "BA_inc_cm2_day"

# take the median daily increment for all the trees for every  month
rc_bymonth <- aggregate(rc_byday[,c("BA_inc_cm2")], 
                        by=with(rc_byday, 
                                list(month=month)), 
                        FUN=median, na.rm=T)
colnames(rc_bymonth)[2] <- "BA_inc_cm2_day"
head(rc_byday)

######### ######### ######### 
######### LAI       ######### 
######### ######### ######### 
# read in LAI data (source (Luigi, BOM))
# lai_df <- read.csv(paste0(common_folder,'LPJ_allocation_phenology/', 'data/BoM_LAI_OzFLUX.csv'), header = T)
lai_robson <- read.csv(paste0(path, 'data/lai_robson_BOM_V2.csv'), header = T)
lai_robson <- lai_robson[lai_robson$veg == "total",][,c(2,3)]
lai_robson <- add_BOM_LAI_stff_V2(lai_robson)
head(lai_robson)
min(lai_robson$Date)
max(lai_robson$Date)

robson_LAI_inc_byyear <- summaryBy(cbind(lai, lai_inc) ~ year+month,
                                   data=lai_robson, FUN=median, keep.names=T)
# lai_cup_bymonth <- summaryBy(lai ~ month,
#                              data=lai_cup_byyear, FUN=median, keep.names=T)
# NOTE: in this case, there is only monthly observations so I am taking 
# the mean across the years per month
robson_LAI_inc_bymonth <- summaryBy(cbind(lai, lai_inc) ~ month,
                                    data=robson_LAI_inc_byyear, FUN=median, keep.names=T)
# calculate hoe many deatapoints we have per month (to make sure that there is no bias
# towards months with less data)
LAI_count_per_month_robson <- summaryBy(lai_inc ~ month,
                                        data=lai_robson, FUN=length, keep.names=T)
# sumarise standard error
LAI_stdev_per_month_robson <- summaryBy(lai_inc ~ month,
                                        data=lai_robson, FUN=sd, keep.names=T)
# make a data frame with these statistics
robson_LAI_montly_summary <- cbind(robson_LAI_inc_bymonth, 
                                   LAI_stdev_per_month_robson[2], 
                                   LAI_stdev_per_month_robson[2])
colnames(robson_LAI_montly_summary)[c(4,5)] <- c("lai_inc_stdev", "lai_inc_count")
# calculate standard error
robson_LAI_montly_summary$stderr <- robson_LAI_montly_summary$lai_inc_stdev / 
  sqrt(robson_LAI_montly_summary$lai_inc_count)

# ggplot(data=robson_LAI_inc_byyear)+
#   geom_boxplot(aes(x=as.factor(month), y=lai_inc), outlier.shape = NA)+
#   ylim(-0.06, 0.06)

########## ########## ########## 
##########   FLUXES.  ########## 
########## ########## ########## 
# read in the flux data (source: Clare Stephens)
load(paste0(path, 'data/data_RobsonCreek.RData'))
flux_data_robson <- site_data
rm(site_data)
head(flux_data_robson)
# add month/year/season columns to flux data
flux_data_robson <- addFLUXstuff(flux_data_robson)
# convert NEE to NEP
flux_data_robson$NEE <- -flux_data_robson$NEE
colnames(flux_data_robson)[6] <- "NEP"
# clip the flux data that it will be whole years
flux_data_robson <- flux_data_robson[flux_data_robson$date <= "2023-07-31",]
# create a timeseries (median for all the dates axcross all traps)
flux_data_robson_byyear <- aggregate(flux_data_robson[,c(2,3,4,6,8)], 
                                     by=with(flux_data_robson, 
                                             list(year=year,
                                                  month=month)), 
                                     FUN=sum, na.rm=T)
head(flux_data_robson_byyear)
Medians_robson_byyear <- aggregate(flux_data_robson[,c(7,10,9)], 
                                   by=with(flux_data_robson, 
                                           list(year=year,
                                                month=month)), 
                                   FUN=median, na.rm=T)

colnames(Medians_robson_byyear)[3] <- "Ta"
head(Medians_robson_byyear)
flux_data_robson_byyear <- cbind(flux_data_robson_byyear, Medians_robson_byyear[,3:5])
# summarised (by year and then by month)
flux_data_robson_bymonth <- aggregate(flux_data_robson_byyear[,c(3:10)], 
                                      by=with(flux_data_robson_byyear, 
                                              list(month=month)), 
                                      FUN=median, na.rm=T)
