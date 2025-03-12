# DENDROMETER DATA.
# SOURCE: NINA J H
# SITE: Wombat State Forest
# DETAILS: Hhere is the Dendro data from 2 different data sets from the Wombat 
# State Forest - 2013-2018 (one 5 years, the other 6 years).
# > Dendro_Wombat_Flux-sites - given you got data from Anne, I provided the whole 
#   timeseries up to 2018 (varying crown classes, 3 species)
# > Dendro_Wombat_FESA - dendro data from the Control plots - these trees are all 
#   from the dominant/co-dominant crown class, 2 species)

common_folder <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
setwd(paste0(common_folder,'LPJ_allocation_phenology/'))

# load functions that are common to other R codes
source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/common_functions.R')

####################################
######## FLUX TOWER PLOT BA ########
####################################
# read in the basal area increment data (source: Nina HN (also includes Anne Griebel)
Wombat_NHN_fluxsite_raw_fromfile <- read.csv(paste0(common_folder,
                                  'LPJ_allocation_phenology/data/Dendro_Wombat_Flux-sites_2012_2018.csv'),
                           header = TRUE)
Wombat_NHN_fluxsite_raw_fromfile$date <- as.Date(Wombat_NHN_fluxsite_raw_fromfile$date, format="%d/%m/%Y")
# head(Wombat_NHN_fluxsite_raw_fromfile)

# remove unecessary data
head(Wombat_NHN_fluxsite_raw_fromfile[,c(1,3,5,8,9,11,13,16)])
Wombat_NHN_fluxsite_raw <- Wombat_NHN_fluxsite_raw_fromfile[,c(1,3,5,8,9,11,13,16)]
# start working on this file
Wombat_NHN_fluxsite <- Wombat_NHN_fluxsite_raw
head(Wombat_NHN_fluxsite)
max(Wombat_NHN_fluxsite$date)
# prepare the flux-site BAI data
Wombat_NHN_fluxsite_new <- prepare_wombat_inc_NHN_raw_data_new(Wombat_NHN_fluxsite)
# Wombat_NHN_fluxsite_new <- prepare_wombat_inc_NHN_raw_data(Wombat_NHN_fluxsite)
head(Wombat_NHN_fluxsite_new)

#################################
######## CONTROL PLOT BA #######
###############################
# read in the basal area increment data (source: Nina HN)
Wombat_NHN_control_raw_fromfile <- 
  read.csv(paste0(common_folder, 
                  'LPJ_allocation_phenology/data/Dendro_Wombat_FESA-Control_2013_2018.csv'), 
           header = TRUE)
head(Wombat_NHN_control_raw_fromfile)
# set date column to workable date
Wombat_NHN_control_raw_fromfile$date <- as.Date(Wombat_NHN_control_raw_fromfile$date, format="%d/%m/%Y")
# create tree ID from other variables
Wombat_NHN_control_raw_fromfile$Tree_ID <- paste0(
  Wombat_NHN_control_raw_fromfile$ST_ID,'_',
  Wombat_NHN_control_raw_fromfile$Tree_nr,'_',
  Wombat_NHN_control_raw_fromfile$Treatment)
# select impostant columns(order is important)
Wombat_NHN_control_raw <- Wombat_NHN_control_raw_fromfile[,c(7, 17, 5, 8, 6, 9, 10, 14)]
head(Wombat_NHN_control_raw)
# change one of the columns to a "crown_class" column (to be able to combine the two datasets)
Wombat_NHN_control_raw$Code <- NA
colnames(Wombat_NHN_control_raw)[5] <- "crown_class"
# NOTE: Assuming all the trees are Dominant (according to Nina, the "control" dataset
# trees are all Dominant or Dominant)
Wombat_NHN_control_raw$crown_class <- "D"
colnames(Wombat_NHN_control_raw)[3] <- "Species"

# calculate BA increment per day
Wombat_NHN_control_new <- prepare_wombat_inc_NHN_raw_data_new(Wombat_NHN_control_raw)
head(Wombat_NHN_control_new)
#################################
########  COMBINED DATA ########
###############################
Wombat_NHN_fluxsite_new$experiment <- "fluxsite"
Wombat_NHN_control_new$experiment <- "control"
# combine dataframes
Wombat_NHN_combined <- rbind(Wombat_NHN_fluxsite_new, Wombat_NHN_control_new)
# remove spaces from the species column
Wombat_NHN_combined$Species <- gsub(" ", "", Wombat_NHN_combined$Species, fixed = TRUE)
# add a month and year column
Wombat_NHN_combined$month <- month(Wombat_NHN_combined$date)
Wombat_NHN_combined$year <- year(Wombat_NHN_combined$date)
# remove the "days" column (not Necessary)
Wombat_NHN_combined <- Wombat_NHN_combined[,-4]
# filter for only the Dominant and Subdominant trees
Wombat_NHN_combined_SDD <-
  Wombat_NHN_combined[Wombat_NHN_combined$crown_class == "D" |
                        Wombat_NHN_combined$crown_class == "SD" , ]

print(paste0("number of all trees: ", 
             length(unique(Wombat_NHN_combined$Tree_ID))))
print(paste0("number of only dominant and subdominant trees: ", 
             length(unique(Wombat_NHN_combined_SDD$Tree_ID))))

head(Wombat_NHN_combined)

######
# create a monthly aggregated time series (years and months)
#    - All trees
Wombat_NHN_combined_timeseries_byYear <- aggregate(Wombat_NHN_combined[,c(7:9)], 
                                     by=with(Wombat_NHN_combined, 
                                             list(year=year,
                                                  month=month)), 
                                     FUN=median, na.rm=T)
head(Wombat_NHN_combined_timeseries_byYear)

#calculate monthly increments from daily increments
Wombat_NHN_combined_timeseries_byYear_withmonthly <- calc_monthly_incs(Wombat_NHN_combined_timeseries_byYear)
head(Wombat_NHN_combined_timeseries_byYear_withmonthly)

Wombat_NHN_combined_timeseries_bymonth_withmonthly <- 
  aggregate(Wombat_NHN_combined_timeseries_byYear_withmonthly[,c(3:8)], 
          by=with(Wombat_NHN_combined_timeseries_byYear_withmonthly, 
                  list(month=month)), 
          FUN=median, na.rm=T)

# colnames(Wombat_NHN_combined_timeseries_byYear)[3] <- "BAI_cm2_day"
head(Wombat_NHN_combined)
Wombat_NHN_combined_timeseries_per_tree_byYear <- aggregate(Wombat_NHN_combined[,c(7:9)], 
                                                   by=with(Wombat_NHN_combined, 
                                                           list(year=year,
                                                                month=month,
                                                                tree=Tree_ID,
                                                                crown_class=crown_class)), 
                                                   FUN=median, na.rm=T)
head(Wombat_NHN_combined_timeseries_per_tree_byYear)
# colnames(Wombat_NHN_combined_timeseries_per_tree_byYear)[4] <- "BAI_cm2_day"

Wombat_NHN_combined_timeseries_per_tree_byYear_withmonthly <- 
  calc_monthly_incs(Wombat_NHN_combined_timeseries_per_tree_byYear)
head(Wombat_NHN_combined_timeseries_per_tree_byYear_withmonthly)

ggplot(data=Wombat_NHN_combined_timeseries_byYear_withmonthly)+
  geom_boxplot(aes(x=as.factor(month), y=BAI_cm2_month))+
  geom_line(data=Wombat_NHN_combined_timeseries_per_tree_byYear_withmonthly, 
            aes(x=month, y=BAI_cm2_month, col=crown_class), alpha=0.8)+
  geom_point(data=Wombat_NHN_combined_timeseries_per_tree_byYear_withmonthly, 
            aes(x=month, y=BAI_cm2_month, col=crown_class), alpha=0.25)


# add a date column for plotting
Wombat_NHN_combined_timeseries_byYear <- 
  add_date_column_from_day_month_year(Wombat_NHN_combined_timeseries_byYear)
head(Wombat_NHN_combined_timeseries_byYear)

# create a monthly aggregated time series (years and months) 
#     - only SubDom and Dom trees
Wombat_NHN_combined_timeseries_byYear_SDD <- aggregate(Wombat_NHN_combined_SDD[,7], 
                                                   by=with(Wombat_NHN_combined_SDD, 
                                                           list(year=year,
                                                                month=month)), 
                                                   FUN=median, na.rm=T)
colnames(Wombat_NHN_combined_timeseries_byYear_SDD)[3] <- "BAI_cm2_day"
Wombat_NHN_combined_timeseries_per_tree_byYear_SDD <- aggregate(Wombat_NHN_combined_SDD[,7], 
                                                            by=with(Wombat_NHN_combined_SDD, 
                                                                    list(year=year,
                                                                         month=month,
                                                                         tree=Tree_ID)), 
                                                            FUN=median, na.rm=T)
colnames(Wombat_NHN_combined_timeseries_per_tree_byYear_SDD)[4] <- "BAI_cm2_day"

# add a date column for plotting
Wombat_NHN_combined_timeseries_byYear_SDD <- 
  add_date_column_from_day_month_year(Wombat_NHN_combined_timeseries_byYear_SDD)
head(Wombat_NHN_combined_timeseries_byYear_SDD)

#######################################################
########  AGGREGATE AND SUMMARISE COMBINED DF ########
#####################################################
# create a monthly aggregated timeseries for the three species (years and months)
#    - All trees
Wombat_NHN_combined_timeseries_bySpecies <- aggregate(Wombat_NHN_combined[,7], 
                                            by=with(Wombat_NHN_combined, 
                                                    list(year=year,
                                                         month=month,
                                                         Species=Species)), 
                                            FUN=median, na.rm=T)
colnames(Wombat_NHN_combined_timeseries_bySpecies)[4] <- "BAI_cm2_day"
# add a date column for plotting
Wombat_NHN_combined_timeseries_bySpecies <- 
  add_date_column_from_day_month_year(Wombat_NHN_combined_timeseries_bySpecies)
head(Wombat_NHN_combined_timeseries_bySpecies)

# absolute monthy (median) values of daily BA increments 
Wombat_NHN_combined_byMonth <- aggregate(Wombat_NHN_combined_timeseries_byYear[,3], 
                                      by=with(Wombat_NHN_combined_timeseries_byYear, 
                                              list(month=month)), 
                                      FUN=median, na.rm=T)
colnames(Wombat_NHN_combined_byMonth)[2] <- "BAI_cm2_day"
# find relative monthly (median) values of daily BA increments
relative_Wombat_NHN_combined_byMonth <- create_relative_dataframe_of_monthly_values_v1(Wombat_NHN_combined_byMonth)

