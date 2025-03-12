library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(doBy)
library(TTR)
library(patchwork) # to plot several plots as panels

rm(list = ls())

common_folder_spider <- '~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/'

source(paste0(common_folder_spider, 'common_functions.R'))

source(paste0(common_folder_spider, 'Wombat_phenology_Griebel.R'))

source(paste0(common_folder_spider, 'wombat_dendro.R'))

source(paste0(common_folder_spider, 'CUP_phenology.R'))

# DBH INCREMENT CUP
BA_inc_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 4)
# LAI CUP
LAI_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 2)
# LAI_INCREMENT CUP
LAI_INC_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 3)
# NEP CUP
NEP_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 9)
# GPP CUP
GPP_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 8)
# ecosystem respiration CUP
ER_rel_cup <-  create_monthly_variable_spider(cup_relative_montly_df, 10)
# soil moisture (EucFACE)
SWC_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 14)
# soil moisture (cup)
SWS_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 13)
# air temp
Tair_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 11)
# insolation
INSOL_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 12)
# Precip 
Precip_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 6)
# ET
ET_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 7)
##### 
# create a df with columns: month; rows: variable (relative).
# note that the first 2 rows are max and min values (1, 0).
Productivity_Responce_cup <- rbind(definebase(), 
                               LAI_INC_rel_cup, 
                               BA_inc_rel_cup, 
                               GPP_rel_cup, 
                               NEP_rel_cup)
rownames(Productivity_Responce_cup) <- c("max","min","LAI_INC","BA inc","GPP","NEP")

colors <- c("darkolivegreen4", "darkorange4", "darkgoldenrod1", "cadetblue4")
titles <- c("LAI_INC","BA inc","GPP","NEP")

#######

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,2))

# Create the radar chart
for(i in 1:4){
  radarchart(Productivity_Responce_cup[c(1,2,i+2),],
             title = titles[i],
             cglty = 1, pcol = colors[i], 
             pfcol = scales::alpha(colors[i], 0.2),
             cglcol = "gray",
             plwd = 2,        # Width of the line
             plty = 1)
}
par(op)
 
###

Productivity_Responce_forcing_cup <- 
  rbind(definebase(), 
        LAI_INC_rel_cup, 
        BA_inc_rel_cup, 
        GPP_rel_cup, 
        NEP_rel_cup, 
        ER_rel_cup, 
        SWS_rel_cup, 
        Tair_rel_cup, 
        INSOL_rel_cup,
        Precip_rel_cup, 
        ET_rel_cup)
rownames(Productivity_Responce_forcing_cup) <- c("max","min","LAI_INC","BA inc",
                                             "GPP","NEP","ER","SMC","Ta","INSOL",
                                             "Precip", "ET")
colors <- c("darkolivegreen4", "darkorange4", "darkgoldenrod1", "cadetblue4",
            "blue1", "aquamarine", "magenta", "red3", "blue4", "blue4")
titles <- c("LAI_INC","BA inc","GPP","NEP","ER","SMC","Ta","INSOL","Precip", "ET")

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(4,3))

# Create the radar chart
for(i in 1:10){
  radarchart(Productivity_Responce_forcing_cup[c(1,2,i+2),],
             title = titles[i],
             cglty = 1, pcol = colors[i], 
             pfcol = scales::alpha(colors[i], 0.2),
             cglcol = "gray",
             plwd = 2,        # Width of the line
             plty = 1)
}
par(op)

########

# LAI Wombat
LAI_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 2)
# LAI_INCREMENT Wombat
LAI_INC_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 3)
# BA_INCREMENT Wombat (note - using Nina's data (2013-2018 tthat include's Anne's))
BA_INC_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_NHN_combined_byMonth, 2)
# PAI_INCREMENT Wombat
PAI_INC_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 5)
# NEP Wombat
NEP_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 9)
# GPP Wombat
GPP_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 8)
# ecosystem respiration Wombat
ER_rel_Wombat <-  create_monthly_variable_spider(relative_Wombat_montly_df, 10)
# soil moisture Wombat
SWC_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 12)
# air temp Wombat
Tair_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 13)
# insolation Wombat
INSOL_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 11)
# Precip Wombat
Precip_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 6)
# ET
ET_rel_Wombat <- create_monthly_variable_spider(relative_Wombat_montly_df, 7)

Productivity_Responce_forcing_Wombat <- 
  rbind(definebase(), 
        LAI_INC_rel_Wombat, 
        BA_INC_rel_Wombat, 
        GPP_rel_Wombat, 
        NEP_rel_Wombat, 
        ER_rel_Wombat, 
        SWC_rel_Wombat, 
        Tair_rel_Wombat, 
        INSOL_rel_Wombat, 
        Precip_rel_Wombat,
        ET_rel_Wombat)
rownames(Productivity_Responce_forcing_Wombat) <- c("max", "min", "LAI_INC", 
                                                    "BA inc", "GPP", "NEP", 
                                                    "ER", "SMC", "Ta", "INSOL",
                                                    "Precip", "ET")
colors <- c("darkolivegreen4", "darkorange4", "darkgoldenrod1", "cadetblue4",
            "blue1", "aquamarine", "magenta", "red3", "blue4", "blue4")
titles <- c("LAI_INC","BA inc","GPP","NEP","ER","SMC","Ta","INSOL","Precip","ET")

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(4,3))

# Create the radar chart
for(i in 1:10){
  radarchart(Productivity_Responce_forcing_Wombat[c(1, 2, i+2),],
             title = titles[i],
             cglty = 1, pcol = colors[i], 
             pfcol = scales::alpha(colors[i], 0.2),
             cglcol = "gray",
             plwd = 2,        # Width of the line
             plty = 1)
}
par(op)

####



# soil moisture (EucFACE)
SWC_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 13)
# soil moisture (cup)
SWS_rel_cup <- create_monthly_variable_spider(cup_relative_montly_df, 14)
SM_cup_spider <- rbind(definebase(), SWC_rel_cup, SWS_rel_cup)
rownames(SM_cup_spider) <- c("max", "min", "SM Neutron", "SM cup")
colors <- c("blue4", "blue4")
titles <- c("SM Neutron", "SM cup")

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2,1))

# Create the radar chart
for(i in 1:2){
  radarchart(Productivity_Responce_forcing_Wombat[c(1, 2, i+2),],
             title = titles[i],
             cglty = 1, pcol = colors[i], 
             pfcol = scales::alpha(colors[i], 0.2),
             cglcol = "gray",
             plwd = 2,        # Width of the line
             plty = 1)
}
par(op)


##### ABSOLUTE VALUES
# for CUP: cup_bymonth_df
# for wombat: Wombat_bymonth_df

