### CURRENTLY LOOKING AT CUMBERLAND PLAINS ONLY

# WORK ON THE OXFLUX DATA
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(lubridate)
library(purrr)
library(ggthemes)
library(fmsb) # spider plots
library(doBy)

common_folder <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
setwd(paste0(common_folder,'LPJ_allocation_phenology/'))

# load stem diameter increment EucFACE data (David Ellsworth) with error correction
source(paste0(common_folder,'EucFACE/DE_dendro/orginize_data.R'))

# load the calculations of the 
source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/ozflux_phenology.R')

# working dataframes passed here are:
# 1. output_df - stem diameter and basal area increment after cleaning and 
#             keeping dominant and sub-dominant trees (source: David Ellsworth) 
#             with good growth 
# output_df_alltrees - same as above, but with all the trees
# 2. lai_cup - lai data (modis) from CUP (source: Clare Stephens from Luigi (BOM))
# 3. flux_data_cup - flux tower data from CUP (source: Clare Stephens)
# 4. EF_soil_moisture_raw_summarybyDate - soil moisture data from EucFACE (source: Belinda)
# 5. output_df_sumdate - stem diameter/basal area increment data across dates
# 6. output_df_sumdate_0pos - every date (only positive increments)
# 7. output_df_sumdate_CO2trt - every date per treatment (CO2.trt) 
# 8. output_df_sumdate_Ring - every date per ring (CO2.trt) 

# arrange order of seasons in all dataframes
head(flux_data_cup)
head(output_df_sumdate)
head(lai_cup)

flux_data_cup$Season <-
  factor(flux_data_cup$Season,
         levels = c("SUMMER", "AUTUMN","WINTER", "SPRING"))
output_df_sumdate$Season <-
  factor(output_df_sumdate$Season,
         levels = c("SUMMER", "AUTUMN","WINTER", "SPRING"))
lai_cup$Season <-
  factor(lai_cup$Season,
         levels = c("SUMMER", "AUTUMN","WINTER", "SPRING"))

#########################################
##### RELATIVE VALUE (0-1) BY MONTH #####
#########################################

common_folder <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
source(paste0(common_folder, 'LPJ_allocation_phenology/monthly_summaries_cup.R'))
# working dataframes passed here are:
# 1. cup_bymonth_df
# 2. cup_relative_montly_df
# 3. cup_relative_montly_df_rows - reshaped relative dataframe  that  has 3 
#       columns: month, variable and Value.
# 4. cup_relative_montly_df_rows_years_months - relative dataframe  that  has 4 
#       columns: year, month, variable and Value.
# 5. ... and the individual monthly summery for each dataset
