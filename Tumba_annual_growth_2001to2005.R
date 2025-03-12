# working with the annual diameter in Tumba (not necesseraly the dendro data)

library(ggplot2)
library(doBy)
library(dplyr)
library(tidyr)
library(tidyverse)
library(patchwork) # to plot several plots as panels
library(readxl)

rm(list = ls())
# define files and folders
filename_tumba <- 
  "data/Tumbarumba_Flux tower forest plots_data 1.csv" # plots (of 0.1ha in size)
filename_tumba_coreha <- 
  "data/Tumbarumba_Wet_Eucalypt_diameter_height_biomass_dat_IbRz0nd.csv" # core hectre

folder <- 
  "~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
# set working dir
setwd(folder)

########################
####### FUNCTIONS ######
########################

# NOT_USED ! 
# calculates the biomass increment per year from annual inventory data (Tumba)
calculate_biomass_increment_per_plot <- function(inDF){
  # inDF <- biomass_per_plot_ha_year
  # plots
  plots <- unique(inDF$Plot)
  # add a biomass inc column
  inDF$biomass_kg_inc <- NA
  # create an empty output df
  out_df <- inDF[inDF$Plot == "YERTYR",]
  # loop through the plot to calculate biomass increment
  for (p in plots) {
    # make copy of plot p
    thisplot <- inDF[inDF$Plot == p,]
    # number of years of data for plot p
    numberofyears <- dim(thisplot)[1]
    # just only if plot p has more than 1 years of measurements 
    # (cannot have increment with only one measurement)
    if (numberofyears > 1) {
      for (d in 2:numberofyears){
        # go through the dates in each plot, and calculate the number of days between each measurements
        thisdate     <- thisplot$Date[d]
        previousdate <- thisplot$Date[d-1]
        daysbetween  <- as.numeric(thisdate - previousdate)
        # increment per day multiplied by the number of days per year
        thisplot$biomass_kg_inc[d] <- 
          ((thisplot$biomass_kg_ha[d] - thisplot$biomass_kg_ha[d-1]) / daysbetween) * 365.25
      }
    }
    out_df <- rbind(out_df, thisplot)
  }
  return(out_df)
}

#######

# read in Tumba file
raw_tumba_fromfile <- read.csv(paste0(folder, filename_tumba))
# convert date column
raw_tumba_fromfile$Date <- as.Date(raw_tumba_fromfile$Date, format="%d.%m.%y")

head(raw_tumba_fromfile)

# add biomass using power function (plotting AGB vs DBH of the Tumba cencusus from 2015)
# all eucs (Eucalyptus. rubida, E. delegatensis, E.  dalrympleana)
a_euc <- 0.0457
b_euc <- 2.5914
# Ecacia Melanoxylon
a_sw <- 0.2353
b_sw <- 2.25
raw_tumba_fromfile$biomass_kg <- NA
# Euc (?)
# !!! assuming AA, BS and MG are Eucs with the same allometric parameters 
raw_tumba_fromfile[raw_tumba_fromfile$Species=="AA"|
                     raw_tumba_fromfile$Species=="BS"|
                     raw_tumba_fromfile$Species=="MG",]$biomass_kg <-
  with(raw_tumba_fromfile[raw_tumba_fromfile$Species=="AA"|
                            raw_tumba_fromfile$Species=="BS"|
                            raw_tumba_fromfile$Species=="MG",], a_euc *  Dbh_cm ^ b_euc)
# Acacia (?)
# !!! assuming TT, W and SW are all acacias with the same allometric parameters
raw_tumba_fromfile[raw_tumba_fromfile$Species=="TT"|
                     raw_tumba_fromfile$Species=="W"|
                     raw_tumba_fromfile$Species=="SW",]$biomass_kg <-
  with(raw_tumba_fromfile[raw_tumba_fromfile$Species=="TT"|
                            raw_tumba_fromfile$Species=="W"|
                            raw_tumba_fromfile$Species=="SW",], a_sw *  Dbh_cm ^ b_sw)

#### make sure that trees that died (will have NA at a certain year onwards) 
# will get the same value as the previous year

# step 1: make sure that all the trees have 5 observations --> should divide by 5
num_obs_per_tree <- aggregate(raw_tumba_fromfile[,"Date"], by=with(raw_tumba_fromfile, 
                                              list(Plot=Plot,
                                                   Tree=Tree)), FUN=length)

# step 2: go through plots and then through trees and make sure that trees that 
# died (will have NA at a certain year onwards) will get the same value as the 
# previous year..
# name of plots
plots <- unique(raw_tumba_fromfile$Plot)

# create an empty DF for outputs
raw_tumba_fromfile$biomass_kg_inc <- NA
trees_per_plot <- raw_tumba_fromfile[raw_tumba_fromfile$Year == 0,]

for (p in plots){
  thisplot <- raw_tumba_fromfile[raw_tumba_fromfile$Plot == p,]
  
  trees <- unique(thisplot$Tree)
  
  # make sure that if a tree dies, it's biomass will still be counted
  for (t in trees){
    thistree <- thisplot[thisplot$Tree == t, ]
    # make sure that the new df is ordered by Date
    thistree <- thistree[order(thistree$Year), ]
    dates <- unique(thistree$Date)
    # go through the dates and calculate increment per tree. Also make sure that 
    # dead trees are kept accounted for...
    for (d in 1:length(dates)){
      # is the current value of biomass a NA (assuming tree is dead)?
      if (is.na(thistree$biomass_kg[d])){
        # take the value from the previous timestep
        thistree$biomass_kg[d] <- thistree$biomass_kg[d-1]
      }
      # starting from the second timestep, calculate the biomass increment
      if (d > 1){
        # go through the dates in each plot, and calculate the biomass increment
        thisdate     <- thistree$Date[d]
        previousdate <- thistree$Date[d-1]
        daysbetween  <- as.numeric(thisdate - previousdate)
        # increment per day multiplied by the number of days per year (kg/0.1ha/yr)
        thistree$biomass_kg_inc[d] <- 
          ((thistree$biomass_kg[d] - thistree$biomass_kg[d-1]) / daysbetween) * 365.25
      }
    } # finished going over all the dates for each tree
    trees_per_plot <- rbind(trees_per_plot, thistree)
  } # finished going through all the trees for this plot
} # finioshed going through all the plots in the df

ggplot(data=trees_per_plot)+
  geom_line(aes(x=Year, y=biomass_kg_inc, color=Tree), show.legend = FALSE)+
  facet_wrap(.~Plot, scales = "free_y")+
  theme_bw()

ggplot(data=trees_per_plot)+
  geom_line(aes(x=Year, y=biomass_kg, color=Tree), show.legend = FALSE)+
  facet_wrap(.~Plot, scales = "free_y")+
  theme_bw()

#### calculate sum biomass per plot per year (kg/ha)
fun <- function(x){return(sum(x, na.rm=T) * 10)}
biomass_per_plot_ha_year <- aggregate(trees_per_plot[,c("biomass_kg", "biomass_kg_inc")], 
                                  by=with(trees_per_plot, 
                                          list(Date=Date, 
                                               Year=Year,
                                               Plot=Plot)), FUN=fun)

colnames(biomass_per_plot_ha_year)[4:5] <- c("biomass_kg_ha", "biomass_inc_kg_ha_yr")
head(biomass_per_plot_ha_year)

# plot to assess
ggplot(data=biomass_per_plot_ha_year)+
  geom_line(aes(x=Year, y=biomass_kg_ha/1000, color=Plot), show.legend = FALSE)+
  ylab("biomass T/ha")+
  theme_bw()+
  facet_wrap(.~Plot, scales = "free_y")
# plot to assess
ggplot(data=biomass_per_plot_ha_year)+
  geom_line(aes(x=Year, y=biomass_inc_kg_ha_yr/1000, color=Plot))+
  geom_point(aes(x=Year, y=biomass_inc_kg_ha_yr/1000, color=Plot), alpha=2/10)+
  ylab("biomass inc T/ha/Year")+
  theme_bw()
# plot to assess
ggplot(data=biomass_per_plot_ha_year[biomass_per_plot_ha_year$Year > 2001,])+
  geom_boxplot(aes(x=as.factor(Year), y=biomass_inc_kg_ha_yr/1000), fill = "gold4")+
  ylab("biomass inc T/ha/Year")+
  theme_bw()
# plot to assess
ggplot(data=biomass_per_plot_ha_year[biomass_per_plot_ha_year$Year > 2001,])+
  geom_boxplot(aes(x=as.factor(Year), y=biomass_kg_ha/1000), fill = "gold4")+
  ylab("total biomass T/ha")+
  theme_bw()

# find the mean/median of annnual AGB and AGB incremnt across all the sites
biomass_and_biomass_incremnt_per_year <- 
  aggregate(biomass_per_plot_ha_year[,c("biomass_kg_ha", "biomass_inc_kg_ha_yr")], 
            by=with(biomass_per_plot_ha_year, 
                    list(Year=Year)), FUN=median, na.rm=T)

with( biomass_and_biomass_incremnt_per_year[biomass_and_biomass_incremnt_per_year$Year>2001,],
      plot(Year, biomass_inc_kg_ha_yr/1000, type="b"), 
      labs(c("year","biomass inc T/ha/Year") ) )
#################################################################################

#################################
########## CORE HECTRE ##########
#################################
# read in Tumba file
raw_tumba_fromfile <- read.csv(paste0(folder, filename_tumba_coreha))

biomass_t_ha_live_trees <- sum(raw_tumba_fromfile$aboveGroundBiomass_kilograms, na.rm=T) / 1000
biomass_t_ha_dead_trees <- sum(raw_tumba_fromfile$standingDeadAboveGroundBiomass_kilograms, na.rm=T) / 1000

total_biomass_t_ha <- biomass_t_ha_live_trees + biomass_t_ha_dead_trees

# plot distribution
ggplot(data=biomass_per_plot_ha_year[biomass_per_plot_ha_year$Year > 2001,])+
  geom_density(aes(biomass_kg_ha/1000, 
                   # fill=as.factor(Year), 
                   color=as.factor(Year)), alpha=0.2)+
  geom_vline(xintercept = total_biomass_t_ha, linetype="dotted", 
             color = "blue", size=1.5)+
  xlab("plot biomass T/ha")+
  theme_bw()


