rm(list = ls())
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

source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/Wombat_phenology.R')



###################
###### PLOT #######
###################
ggplot(data=Wombat_BAI_df)+
  geom_line(aes(x=date, y=BA_inc_cm, color=species))+
  theme_bw()+
  facet_wrap(.~canopy_class)

ggplot(data=Wombat_BAI_df)+
  geom_boxplot(aes(x=as.factor(month), y=BA_inc_cm))+
  theme_bw()+
  ylim(-.5,.5)


relative_Wombat_montly_df_rows$Variable <-
  factor(relative_Wombat_montly_df_rows$Variable,
         levels = c("Ta", "Fsd", "Sws", "precip", "ET", 
                    "GPP", "NEP", "ER", "lai", "lai_inc", "BA_inc_cm","pai_inc"))
relative_montly_wombat_rows_years_months$Variable <-
  factor(relative_montly_wombat_rows_years_months$Variable,
         levels = c("Ta", "Fsd", "Sws", "precip", "ET", 
                    "GPP", "NEP", "ER", "lai", "lai_inc", "BA_inc_cm","pai_inc"))

ggplot(data=relative_Wombat_montly_df_rows)+
  geom_line(aes(x=month, y=Value, col=Variable))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()

ggplot(data=relative_montly_wombat_rows_years_months)+
  geom_boxplot(aes(x=as.factor(month), y=Value))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()

ggplot(data=relative_montly_wombat_rows_years_months)+
  geom_boxplot(aes(x=as.factor(month), y=Value))+
  geom_line(data=relative_Wombat_montly_df_rows, aes(x=month, y=Value, col=Variable))+
  geom_point(data=relative_Wombat_montly_df_rows, aes(x=month, y=Value, col=Variable))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()

absolute_Wombat_montly_df_rows$Variable <-
  factor(absolute_Wombat_montly_df_rows$Variable,
         levels = c("Ta", "Fsd", "Sws", "precip", "ET", 
                    "GPP", "NEP", "ER", "lai", "lai_inc", "BA_inc_cm","pai_inc"))

# absolute values (boxplot)
ggplot(data=absolute_Wombat_montly_df_rows)+
  geom_line(aes(month, y=Value, color=Variable))+
  geom_point(aes(month, y=Value))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
  xlim(1,12)+
  scale_x_continuous(breaks=seq(1,12,1))

