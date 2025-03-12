
# Basal area increments: Source: Griebel (DO NOT SEND OR PUBLISH WITHOUT PERMISSION. CO-AUTHORSHIP PROMISSED)
# Data is from 72 trees in total, covering 3 eucalypt species and 4 canopy classes
# Measurements were collected from 1 Dec 2012 – 31 Dec 2015
# Increments were converted to mm2 from approximately fortnightly readings of band dendrometers (0.1mm accuracy).
# I’ve included the Species name, canopy class and starting DBH (above bark) for size reference of individual trees
# Canopy classification according to Smith, D.M., 1986. The Practice of Silviculture. John Wiley & Sons, New York, p. 527.
# S  = suppressed
# I  = Intermediate
# SD = Subdominant
# D  = Dominant
# Reference publication: http://dx.doi.org/10.1016/j.foreco.2016.12.017

# LAI: source: LUIGI (BOM)

# Fluc data: source: Clare Stephens


common_folder <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
setwd(paste0(common_folder,'LPJ_allocation_phenology/'))

# load functions that are common to other R codes
source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/common_functions.R')


######################### 
#### BASAL AREA DATA ####
######################### 
# read in the basal area increment data (source: Anne Griebel)
Wombat_BAI_raw <- read.csv(paste0(common_folder,
         'LPJ_allocation_phenology/data/Griebel_cumulative_tree_BAI_WombatStateForest.csv'),
                           header = TRUE)
# remove first column
Wombat_BAI <- Wombat_BAI_raw[,-c(1)]
# remove last row
Wombat_BAI <- Wombat_BAI[1:101,]
# create a new df that we can work with
# also calcualte the actual BA increment
Wombat_BAI_df <- create_new_BAIdf_wombat_V1(Wombat_BAI)
head(Wombat_BAI_df)
# summarise DBH_inc/BA_inc by month
# leave only positive increments  (or 0)
# Wombat_BAI_df_nonegative <- Wombat_BAI_df[Wombat_BAI_df$BA_inc_cm >= 0, ] 
Wombat_BAI_df_nonegative <- Wombat_BAI_df
# remove canopy classes if needed:
# S  = suppressed
# I  = Intermediate
# SD = Subdominant
# D  = Dominant
#!!!!!! ---->  COMMENT: STAYED ONLY WITH SUBDOMINANT AND DOMINANT <------ !!!!!!
Wombat_BAI_df_nonegative <- 
  Wombat_BAI_df_nonegative[(Wombat_BAI_df_nonegative$canopy_class == "D" |
                              Wombat_BAI_df_nonegative$canopy_class == "SD"), ]

# # remove NA from month
# temp_pos_DBH_inc_pos <- temp_pos_DBH_inc_pos[!is.na(temp_pos_DBH_inc_pos$month),] 
# summarise by year and month
Wombat_BA_inc_byyear <- summaryBy(BA_inc_cm ~ month+year,
                                  data=Wombat_BAI_df_nonegative, FUN=median, keep.names=T)
Wombat_BA_inc_per_tree_byyear <- summaryBy(BA_inc_cm ~ month+year+treeID,
                                  data=Wombat_BAI_df_nonegative, FUN=median, keep.names=T)

# just to know how many reps we get per month (along the years).
Wombat_BA_inc_byyear_stats <- summaryBy(BA_inc_cm ~ month+year,
                                        data=Wombat_BAI_df_nonegative, FUN=length, keep.names=T)
# DBH_BA_inc_bymonth <- summaryBy(cbind(DBH_inc, BA_inc) ~ month,
#                                 data=DBH_BA_inc_byyear, FUN=median, keep.names=T)
# NOTE: in this case, there is only monthly observations (I made sure of that in 
# "orginize_data.R") so I am taking the mean across the years per month
Wombat_BA_inc_bymonth <- summaryBy(BA_inc_cm ~ month,
                                   data=Wombat_BAI_df_nonegative, FUN=median, keep.names=T)
# calculate hoe many deatapoints we have per month (to make sure that there is no bias
# towards months with less data)
BAI_count_per_month_wombat <- summaryBy(BA_inc_cm ~ month,
                                        data=Wombat_BAI_df_nonegative, FUN=length, keep.names=T)
# sumarise standard error
BAI_stdev_per_month_wombat <- summaryBy(BA_inc_cm ~ month,
                                        data=Wombat_BAI_df_nonegative, FUN=sd, keep.names=T)
# make a data frame with these statistics
wombat_BA_montly_summary <- cbind(Wombat_BA_inc_bymonth, 
                                  BAI_stdev_per_month_wombat[2], 
                                  BAI_count_per_month_wombat[2])
colnames(wombat_BA_montly_summary)[c(3,4)] <- c("stdev", "count")
# calculate standard error
wombat_BA_montly_summary$stderr <- wombat_BA_montly_summary$stdev / sqrt(wombat_BA_montly_summary$count)

####################
#### FLUX DATA ##### 
####################
# read in the flux data (source: Clare Stephens)
load(paste0(common_folder,'LPJ_allocation_phenology/', 'data/data_WombatStateForest.RData'))
flux_data_wombat <- site_data
rm(site_data)
# add month/year/season columns to flux data
flux_data_wombat <- addFLUXstuff(flux_data_wombat)
# convert NEE to NEP
flux_data_wombat$NEE <- -flux_data_wombat$NEE
colnames(flux_data_wombat)[6] <- "NEP"
# min(flux_data_wombat$date)
# max(flux_data_wombat$date)
# clip the flux data that it will be whole years
flux_data_wombat <- flux_data_wombat[flux_data_wombat$date >= "2012-12-01",]
flux_data_wombat <- flux_data_wombat[flux_data_wombat$date < "2018-12-31",]
head(flux_data_wombat)
# create a timeseries (median for all the dates axcross all traps)
flux_data_wombat_byyear <- aggregate(flux_data_wombat[,c(2,3,4,6,8)], 
                                     by=with(flux_data_wombat, 
                                             list(year=year,
                                                  month=month)), 
                                     FUN=sum, na.rm=T)
Medians_wombat_byyear <- aggregate(flux_data_wombat[,c(7,10,9)], 
                                    by=with(flux_data_wombat, 
                                            list(year=year,
                                                 month=month)), 
                                    FUN=median, na.rm=T)

colnames(Medians_wombat_byyear)[3] <- "Ta"
flux_data_wombat_byyear <- cbind(flux_data_wombat_byyear, Medians_wombat_byyear[,3:5])
# summarised (by year and then by month)
flux_data_wombat_bymonth <- aggregate(flux_data_wombat_byyear[,c(3:10)], 
                                      by=with(flux_data_wombat_byyear, 
                                              list(month=month)), 
                                      FUN=median, na.rm=T)
flux_data_wombat_bymonth
##################################
####### PAI DATA (GRIEBEL) #######
##################################

filename_PAVD_PAI <- 'Griebel_LAI_PAVD_WombatStateForest.csv'
# read in the basal area increment data (source: Anne Griebel)
Wombat_PAVD_PAI_raw <- read.csv(paste0(common_folder,
                                       'LPJ_allocation_phenology/data/', filename_PAVD_PAI),
                                header = TRUE)

# create a PAI dataframe from the raw data. Also add year, month and PAI_Increment
Wombat_PAI <- create_new_PAIdf_wombat_V1(Wombat_PAVD_PAI_raw)

# summarise by year and month
Wombat_PAI_inc_byyear <- summaryBy(pai_inc ~ month+year,
                                   data=Wombat_PAI, FUN=median, keep.names=T)
# just to know how many reps we get per month (along the years).
Wombat_PAI_inc_byyear_stats <- summaryBy(pai_inc ~ month+year,
                                         data=Wombat_PAI, FUN=length, keep.names=T)

# NOTE: in this case, there is only monthly observations (I made sure of that in 
# "orginize_data.R") so I am taking the mean across the years per month
Wombat_PAI_inc_bymonth <- summaryBy(pai_inc ~ month,
                                    data=Wombat_PAI, FUN=median, keep.names=T)
# calculate hoe many deatapoints we have per month (to make sure that there is no bias
# towards months with less data)
PAI_count_per_month_wombat <- summaryBy(pai_inc ~ month,
                                        data=Wombat_PAI, FUN=length, keep.names=T)
# sumarise standard error
PAI_stdev_per_month_wombat <- summaryBy(pai_inc ~ month,
                                        data=Wombat_PAI, FUN=sd, keep.names=T)
# make a data frame with these statistics
wombat_PAI_montly_summary <- cbind(Wombat_PAI_inc_bymonth, 
                                   PAI_stdev_per_month_wombat[2], 
                                   PAI_count_per_month_wombat[2])
colnames(wombat_PAI_montly_summary)[c(3,4)] <- c("stdev", "count")
# calculate standard error
wombat_PAI_montly_summary$stderr <- wombat_PAI_montly_summary$stdev / sqrt(wombat_PAI_montly_summary$count)

###################
#### LAI DATA ##### 
###################
# read in LAI data (source (Luigi, BOM))
# lai_df <- read.csv(paste0(common_folder,'LPJ_allocation_phenology/', 'data/BoM_LAI_OzFLUX.csv'), header = T)
lai_df <- read.csv(paste0(common_folder,'LPJ_allocation_phenology/', 'data/lai_wombat_BOM_V2.csv'), header = T)
lai_wombat <- lai_df[lai_df$veg == "total",][,c(2,3)]
lai_wombat <- add_BOM_LAI_stff_V2(lai_wombat)
head(lai_wombat)
min(lai_wombat$Date)
max(lai_wombat$Date)
# clip lai data according to the BA data (from Nina)
lai_wombat <- lai_wombat[lai_wombat$Date >= "2012-12-01", ]
lai_wombat <- lai_wombat[lai_wombat$Date <= "2018-12-31", ]


# summarise LAI by month
Wombat_LAI_inc_byyear <- summaryBy(cbind(lai, lai_inc) ~ year+month,
                                   data=lai_wombat, FUN=median, keep.names=T)
head(Wombat_LAI_inc_byyear)

calc_monthly_lai_incs <- function(inDF) {
  # inDF <- Wombat_LAI_inc_byyear
  inDF$lai_inc_month <- NA
  for (i in 1:length(inDF$month)) {
    inDF$lai_inc_month[i] <- inDF$lai_inc[i] * daysinthismonth(inDF$month[i], inDF$year[i])
  }
  return(inDF)
}

Wombat_LAI_inc_byyear_withmonths <- calc_monthly_lai_incs(Wombat_LAI_inc_byyear)
head(Wombat_LAI_inc_byyear_withmonths)

Wombat_LAI_inc_bymonth_withmonths <- aggregate(Wombat_LAI_inc_byyear_withmonths[,c(3:5)], 
                                               by=with(Wombat_LAI_inc_byyear_withmonths, 
                                                       list(month=month)), 
                                               FUN=median, na.rm=T)
# lai_cup_bymonth <- summaryBy(lai ~ month,
#                              data=lai_cup_byyear, FUN=median, keep.names=T)
# NOTE: in this case, there is only monthly observations so I am taking 
# the mean across the years per month
Wombat_LAI_inc_bymonth <- summaryBy(cbind(lai, lai_inc) ~ month,
                                    data=Wombat_LAI_inc_byyear, FUN=median, keep.names=T)
# calculate hoe many deatapoints we have per month (to make sure that there is no bias
# towards months with less data)
LAI_count_per_month_wombat <- summaryBy(lai_inc ~ month,
                                        data=lai_wombat, FUN=length, keep.names=T)
# sumarise standard error
LAI_stdev_per_month_wombat <- summaryBy(lai_inc ~ month,
                                        data=lai_wombat, FUN=sd, keep.names=T)
# make a data frame with these statistics
wombat_LAI_montly_summary <- cbind(Wombat_LAI_inc_bymonth, 
                                   LAI_stdev_per_month_wombat[2], 
                                   LAI_count_per_month_wombat[2])
colnames(wombat_LAI_montly_summary)[c(4,5)] <- c("lai_inc_stdev", "lai_inc_count")
# calculate standard error
wombat_LAI_montly_summary$stderr <- wombat_LAI_montly_summary$lai_inc_stdev / sqrt(wombat_LAI_montly_summary$lai_inc_count)
# remove canopy classes

########################################
###### CREATE A SUMMARY DATAFRAME ######
########################################

# create one dataframe to hold ALL the monthly summaries:
Wombat_bymonth_df <- cbind(wombat_LAI_montly_summary[,c(1:3)], 
                    wombat_BA_montly_summary[2], 
                    wombat_PAI_montly_summary[2], 
                    flux_data_wombat_bymonth[c(2:9)])
head(Wombat_bymonth_df)

# create a dataframe with relative values (ie relative to the maximum in each of the columns)
# relative_Wombat_montly_df <- create_relative_dataframe_of_monthly_values(Wombat_bymonth_df)
relative_Wombat_montly_df <- create_relative_dataframe_of_monthly_values_v1(Wombat_bymonth_df)

#### RESHAPE ACROSS MONTHS  #####
# reshape the absolute dataframe such that it has 3 columns: month, variable and Value.
absolute_Wombat_montly_df_rows <- reshape_cols_to_rows(Wombat_bymonth_df)
# reshape the relative dataframe such that it has 3 columns: month, variable and Value.
relative_Wombat_montly_df_rows <- reshape_cols_to_rows(relative_Wombat_montly_df)

# calculate relative values
relative_Wombat_BA_inc_byyear <- create_relative_dataframe_of_monthly_values_across_years_V2(Wombat_BA_inc_byyear)
# remove the NA (it is only in 2012, where there is only 1 data point)
relative_Wombat_BA_inc_byyear <- relative_Wombat_BA_inc_byyear[!is.na(relative_Wombat_BA_inc_byyear$BA_inc_cm),]

relative_Wombat_LAI_inc_byyear <- create_relative_dataframe_of_monthly_values_across_years_V2(Wombat_LAI_inc_byyear)

relative_Wombat_PAI_inc_byyear <- create_relative_dataframe_of_monthly_values_across_years_V2(Wombat_PAI_inc_byyear)

relative_flux_data_wombat_byyear <- create_relative_dataframe_of_monthly_values_across_years_V2(flux_data_wombat_byyear)

# reshape DBH and BA increment across years and months
relative_Wombat_BA_inc_byyear_reshaped <- reshape_cols_to_rows_year_month(relative_Wombat_BA_inc_byyear)
# reshape LAI across years and months
relative_Wombat_LAI_inc_byyear_reshaped <- reshape_cols_to_rows_year_month(relative_Wombat_LAI_inc_byyear)
# reshape PAI across years and months
relative_Wombat_PAI_inc_byyear_reshaped <- reshape_cols_to_rows_year_month(relative_Wombat_PAI_inc_byyear)
# reshape flux data across years and months
relative_flux_data_wombat_byyear_reshaped <- reshape_cols_to_rows_year_month(relative_flux_data_wombat_byyear)


# bind all reshaped dfs
relative_montly_wombat_rows_years_months <- rbind(relative_Wombat_BA_inc_byyear_reshaped, 
                                                  relative_Wombat_LAI_inc_byyear_reshaped, 
                                                  relative_Wombat_PAI_inc_byyear_reshaped,
                                                  relative_flux_data_wombat_byyear_reshaped)




