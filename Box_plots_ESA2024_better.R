##### ABSOLUTE VALUES
# for CUP: cup_bymonth_df
# for wombat: Wombat_bymonth_df

library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(doBy)
library(patchwork) # to plot several plots as panels
library(TTR) # for smoothing

rm(list = ls())

common_folder_spider <- '~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/'
setwd(common_folder_spider)

source(paste0(common_folder_spider, 'common_functions.R'))

#data from Anne Griebel (dendrometer, LAI, PAI, dendrometers) for wombat
source(paste0(common_folder_spider, 'Wombat_phenology_Griebel.R'))
# main DF that holds flux data for Wombat: flux_data_wombat_byyear
# main DF that holds LAI data for Wombat: Wombat_LAI_inc_byyear_withmonths

# data from Nina (only the dendrometer data, but for longer period) for wombat
source(paste0(common_folder_spider, 'wombat_dendro.R'))
### main output DF for BAinc & RGR: Wombat_NHN_combined_timeseries_byYear_withmonthly

# data from 
source(paste0(common_folder_spider, 'robson_phenology_new.R'))

source(paste0(common_folder_spider, 'CUP_phenology.R'))

source (paste0(common_folder_spider, "arrange_sites_BAI_data_4plotting.R"))



# CONVERT THESE TO ROWS - CUP
# cup_BA_inc_byyear <- cup_DBH_BA_inc_byyear[,-3] # <-- remove DBH Increments
# cup_BA_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(cup_BA_inc_byyear)
cutoff <- 0.5
# Tree that have a daily increment of more than XXX will be removed. a 0.5 cm2/day
# is ~ 4 cm of radius increase per day, which is not reasonable. 
# However, this could be a period of large growth rates, which we need to be 
# mindful of.
BAI_cup <- remove_trees_from_cup(BAI_cup, cutoff)
cup_BA_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(BAI_cup[,c(1,2,4)])
head(cup_BA_inc_byyear_3cols)
unique(cup_BA_inc_byyear_3cols$Variable)
# # Test plot
# ggplot(data=cup_BA_inc_byyear_3cols)+
# geom_boxplot(aes(x=as.factor(month), y=Value))
lai_cup_byyear_3cols <- reshape_yearly_monthly_to_rows(lai_cup_byyear)
flux_data_cup_byyear_3cols <- reshape_yearly_monthly_to_rows(flux_data_cup_byyear)
# EF_soil_moisture_3cols <- reshape_yearly_monthly_to_rows(EF_soil_moisture_byyear)

# testboxplotvar(cup_BA_inc_byyear_3cols,"BAI_cm2_day","cup" )

# CONVERT THESE TO ROWS - Wombat
Wombat_LAI_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(Wombat_LAI_inc_byyear)
Wombat_BA_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(Wombat_BA_inc_byyear)
head(Wombat_BA_inc_byyear)
Wombat_BA_inc_byyear_3cols <-  reshape_yearly_monthly_to_rows(BAI_wombat[,c(1,2,4)])
# Wombat_BA_inc_byyear_3cols[Wombat_BA_inc_byyear_3cols$Variable 
#                            == "BA_inc_cm",]$Variable <- "BA_inc"
flux_data_wombat_byyear_3cols <- reshape_yearly_monthly_to_rows(flux_data_wombat_byyear)

# # CONVERT THESE TO ROWS - robson
robson_LAI_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(robson_LAI_inc_byyear)
robson_BA_inc_byyear_3cols <- reshape_yearly_monthly_to_rows(BAI_robson[,c(1,2,4)])
flux_data_robson_byyear_3cols <- reshape_yearly_monthly_to_rows(flux_data_robson_byyear)

unique(flux_data_robson_byyear_3cols$Variable)

########

# bind the new DFs together - CUP
all_cup_absolute_byYear <- rbind(cup_BA_inc_byyear_3cols,
                                 lai_cup_byyear_3cols,
                                 flux_data_cup_byyear_3cols)
all_cup_absolute_byYear$site <- "CUP"

# bind the new DFs together - Wombat
all_Wombat_absolute_byYear <- rbind(Wombat_LAI_inc_byyear_3cols,
                                    Wombat_BA_inc_byyear_3cols,
                                    flux_data_wombat_byyear_3cols)
all_Wombat_absolute_byYear$site <- "Wombat"

# bind the new DFs together - Robson Ck
all_robson_absolute_byYear <- rbind(robson_LAI_inc_byyear_3cols,
                                    robson_BA_inc_byyear_3cols,
                                    flux_data_robson_byyear_3cols)
all_robson_absolute_byYear$site <- "Robson"

# chgange the order of the df according to variable name - CUP
unique(all_cup_absolute_byYear$Variable)
a <- all_cup_absolute_byYear
sum(is.na(all_cup_absolute_byYear$Variable))
all_cup_absolute_byYear$Variable <-
  factor(all_cup_absolute_byYear$Variable,
         levels = c("BAI_cm2_day", "lai","lai_inc", 
                    "precip", "ET",
                    "GPP", "NEP", "ER", "Ta",
                    "Fsd", "Sws"))
# write.csv(all_cup_absolute_byYear, file="all_cup_absolute_byYear.csv")

# chgange the order of the df according to variable name - Wombat
unique(all_Wombat_absolute_byYear$Variable)
all_Wombat_absolute_byYear$Variable <-
  factor(all_Wombat_absolute_byYear$Variable,
         levels = c("BAI_cm2_day", "lai","lai_inc", 
                    "precip", "ET",
                    "GPP", "NEP", "ER", "Ta",
                    "Fsd", "Sws"))
sum(is.na(all_Wombat_absolute_byYear$Variable))

# testboxplotvar(cup_BA_inc_byyear_3cols,"BAI_cm2_day","cup" )
# write.csv(all_Wombat_absolute_byYear, file="all_Wombat_absolute_byYear.csv")

# chgange the order of the df according to variable name - Robson ck.
unique(all_robson_absolute_byYear$Variable)
all_robson_absolute_byYear$Variable <-
  factor(all_robson_absolute_byYear$Variable,
         levels = c("BAI_cm2_day", "lai","lai_inc", 
                    "precip", "ET",
                    "GPP", "NEP", "ER", "Ta",
                    "Fsd", "Sws"))
sum(is.na(all_robson_absolute_byYear$Variable))

########## NEW PART (ESA2024)

# switch year and month comun for this df
all_cup_absolute_byYear <- all_cup_absolute_byYear[,c(2,1,3,4,5)]

names(all_Wombat_absolute_byYear)
names(all_robson_absolute_byYear)
names(all_cup_absolute_byYear)

all_data_byYear <- rbind(all_robson_absolute_byYear,
                         all_cup_absolute_byYear,
                         all_Wombat_absolute_byYear)

# create boxplot 
