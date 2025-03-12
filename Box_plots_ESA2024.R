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
# main DF that holds flux data for Wombat: flux_data_wombat_byyear, flux_data_wombat_bymonth
# main DF that holds LAI data for Wombat: Wombat_LAI_inc_byyear_withmonths, 
# ... and Wombat_LAI_inc_bymonth_withmonths

# data from Nina (only the dendrometer data, but for longer period) for wombat
source(paste0(common_folder_spider, 'wombat_dendro.R'))
### main output DF for BAinc & RGR: Wombat_NHN_combined_timeseries_byYear_withmonthly
### ...and Wombat_NHN_combined_timeseries_bymonth_withmonthly

# data from Oliver Binks
source(paste0(common_folder_spider, 'robson_phenology_new.R'))
### main output DF for BAinc & RGR: rc_bymonth
### main output DF for LAI: lai_robson
### main output DF for flux data: flux_data_robson_byyear, flux_data_robson_bymonth

source(paste0(common_folder_spider, 'CUP_phenology.R'))
head(BAI_cup) # source:
head(lai_cup_byyear) # source:
head(flux_data_cup_byyear) # source: 

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
head(BAI_cup)
unique(BAI_cup$site)
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

# write.csv(all_robson_absolute_byYear, file="all_robson_absolute_byYear.csv")

###########################
######### PLOTTING #########
###########################

# PLOT - CUP
ggplot(data=all_cup_absolute_byYear)+
  geom_boxplot(aes(x=as.factor(month), y=Value), fill="red3")+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
    xlab("Month")

# PLOT - Wombat
ggplot(data=all_Wombat_absolute_byYear)+
  geom_boxplot(aes(x=as.factor(month), y=Value), fill="blue")+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
  xlab("Month")

# PLOT - Robson Ck
ggplot(data=all_robson_absolute_byYear)+
  geom_boxplot(aes(x=as.factor(month), y=Value), fill="green3")+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
  xlab("Month")

# PLOT - All
all_absolute_byYear <- rbind(all_cup_absolute_byYear, all_Wombat_absolute_byYear, all_robson_absolute_byYear)
unique(all_absolute_byYear$Variable)

la <- list("BA increment (median cm2/day)", "LAI","LAI increment (median LAI/day)", 
        "precip (mm/month)", "ET, (sum, mm/month)",
        "GPP (sum, gC/m2/month)", "NEP (gC/m2/month)", "EcoResp (gC/m2/month)", "Ta (˚c)",
        "SW.rad (median W/m2)", "Soil Moisture (m/m)")

labeller1 <- function(variable,value){
  return(la[value])
}
ggplot(data=all_absolute_byYear)+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=site))+
  facet_wrap(.~Variable, ncol=3, scales = "free_y", labeller = labeller1)+
  theme_bw()+
  xlab("Month")

ggplot(data=all_absolute_byYear)+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=site), outlier.shape = NA)+
  facet_wrap(.~Variable, ncol=3, scales = "free_y", labeller = labeller1)+
  theme_bw()+
  xlab("Month")

ggplot(data=all_absolute_byYear[!all_absolute_byYear$site == 'Robson',])+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=site), outlier.shape = NA)+
  facet_wrap(.~Variable, ncol=3, scales = "free_y", labeller = labeller1)+
  theme_bw()+
  xlab("Month")

ggplot(data=all_absolute_byYear[all_absolute_byYear$Variable == "BAI_cm2_day",])+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=site), outlier.shape = NA)+
  theme_bw()+
  ylim(-0.3, 0.45)+
  xlab("Month")
  
  ggplot(data=all_absolute_byYear[all_absolute_byYear$Variable == "BAI_cm2_day",])+
    geom_boxplot(aes(x=as.factor(month), y=Value, fill=site), outlier.shape = NA)+
    theme_bw()+
    ylim(-0.3, 0.45)+
    xlab("Month")+
    facet_wrap(.~site, scales = "free_y")


  

  #######
  # add a date column
  all_absolute_byYear_withdate <- all_absolute_byYear
  all_absolute_byYear_withdate$date <- paste0(all_absolute_byYear_withdate$year,'-',
                                              all_absolute_byYear_withdate$month, '-',
                                              '01')
  all_absolute_byYear_withdate$date <- as.Date(all_absolute_byYear_withdate$date)
  
  ggplot(data=all_absolute_byYear_withdate[all_absolute_byYear$Variable == "Sws",])+
    geom_line(aes(x=date, y=Value, col=site))+
    theme_bw()+
    # ylim(-0.3, 0.45)+
    xlab("Date")+
    facet_wrap(.~site)
###############
unique(all_absolute_byYear_withdate$Variable)
# write.csv(all_absolute_byYear_withdate, file = "all_absolute_byYear_withdate.csv")
  
  
ggplot()+
  geom_line(data=cup_bymonth_df, aes(x=month,y=GPP/lai), color="red")+
  geom_line(data=Wombat_bymonth_df, aes(x=month,y=GPP/lai), color="blue")+
  # geom_line(data=robson_bymonth_df, aes(x=month,y=GPP/lai), color="green")+
  theme_bw()

### PLOT LINES AS A FUNCTION OF MONTH
  # reshape CUP #
absolute_cup_montly_df_rows <- reshape_cols_to_rows(cup_bymonth_df)
# remove some variables
absolute_cup_montly_df_rows <- 
  absolute_cup_montly_df_rows[!absolute_cup_montly_df_rows$Variable == "swc",]
absolute_cup_montly_df_rows <- 
  absolute_cup_montly_df_rows[!absolute_cup_montly_df_rows$Variable == 
                                "DBH_inc",]
absolute_cup_montly_df_rows[absolute_cup_montly_df_rows$Variable == 
                              "BA_inc",]$Variable <- "BA_inc_cm"
# add site column
absolute_cup_montly_df_rows$site <- "cup"


# reshape WOMBAT #
absolute_Wombat_montly_df_rows <- reshape_cols_to_rows(Wombat_bymonth_df)
# remove some variables
absolute_Wombat_montly_df_rows <- 
  absolute_Wombat_montly_df_rows[!absolute_Wombat_montly_df_rows$Variable == "pai_inc",]
# add site column
absolute_Wombat_montly_df_rows$site <- "Wombat"

# reshape ROBSON #
absolute_Robson_montly_df_rows <- reshape_cols_to_rows(Robson_bymonth_df)
# remove some variables
absolute_Wombat_montly_df_rows <- 
  absolute_Wombat_montly_df_rows[!absolute_Wombat_montly_df_rows$Variable == "pai_inc",]
# add site column
absolute_Wombat_montly_df_rows$site <- "Wombat"

absolute_values_combined <- rbind(absolute_cup_montly_df_rows, absolute_Wombat_montly_df_rows)

gg1 <- ggplot(data=absolute_cup_montly_df_rows)+
  geom_line(aes(x=month, y=Value))+
  geom_point(aes(x=month, y=Value))+
  facet_wrap(.~Variable, scales = "free_y")

gg2 <- ggplot(data=absolute_Wombat_montly_df_rows)+
  geom_line(aes(x=month, y=Value))+
  geom_point(aes(x=month, y=Value))+
  facet_wrap(.~Variable, scales = "free_y")

gg1 / gg2
# change order of the variable
absolute_values_combined$Variable <-
  factor(absolute_values_combined$Variable,
         levels = c("BA_inc_cm", "lai", "lai_inc", "precip", "ET", "GPP" , "NEP", 
                    "ER" ,"Ta", "Fsd", "Sws" ))

ggplot(data=absolute_values_combined)+
  geom_line(aes(x=month, y=Value, color = site))+
  geom_point(aes(x=month, y=Value, color = site))+
  facet_wrap(.~Variable, ncol=3, scales = "free_y", labeller = labeller1)+
  theme_bw()


unique(absolute_values_combined$Variable)

