# Plot Site results for poster

library(plotly)
library(ncdf4)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(plotly)
library(patchwork)
library(lubridate)

rm(list = ls())


site     <- "CapeTribulation"
site     <- "WombatStateForest"
#site     <- "Tumbarumba"
# site     <- "Calperum"
site     <- "CumberlandPlain"
# site <- "WallabyCreek"
# site <- "RobsonCreek"
# site <- "AliceSpringsMulga"
site <- "Warra"

# pft <- "TeBE"
version <- "out"
pft <- "TRspE"
basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"

source("~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/functions.R")


vars_all <-list.files(path = ".") #


# plot timeseries on the same plot
plot_group_x <- function(groupx, out, date, pft) {
  # groupx <- vars_smass_pools
  groupplot <- plot_ly()
  # %>% layout(yaxis = list(range=c(0,1.2)))
  for (var in groupx){
    # plot the variable
    groupplot <- groupplot  %>%
      add_lines(x = date, y = out[[var]][,pft], name = var)
  }
  return(groupplot)
}

# plot timeseries on the same plot
boxplot_group_x <- function(groupx, out, date, pft) {
  # groupx <- vars_smass_pools
  groupplot <- plot_ly() 
  # %>% layout(yaxis = list(range=c(0,1.2)))
  for (var in groupx){
    # plot the variable
    groupplot <- groupplot  %>%
      add_boxplot(x = month, y = out[[var]][,pft], name = var)
  }
  return(groupplot)
}

#####################################################################################################

version <- "out"
group1 <- c("dave_indiv_npp.out", "dave_indiv_gpp.out")
# group2 <- c("dave_lai.out", "dave_meanleafage.out", "dave_wscal.out","dave_cgrow_pot.out", "dave_ftemp.out", "dave_fwater.out", 
#             "dave_fphoto.out","dave_fstorage.out")
group2 <- c("dave_lai.out","dave_wscal.out")
group3 <- c("dave_cmass_sap.out", "dave_cmass_leaf.out", "dave_cmass_root.out", 
            "dave_cmass_repr.out" , "dave_cmass_sap_limit.out", 
            "dave_cmass_leaf_limit.out", "dave_cmass_root_limit.out", "dave_cmass_repr_limit.out")
# group4 <- c("dave_cgrow.out")
group4 <- c("dave_cgrow.out", "dave_cgrow_sap.out", "dave_cgrow_leaf.out", 
            "dave_cgrow_root.out", "dave_cgrow_repr.out",
            "dave_dturnover_sap.out", "dave_dturnover_leaf.out", "dave_dturnover_root.out")
group5 <- c("dave_cmass_storage.out", "dave_cmass_storage_max.out", 
            "dave_cmass_storage_dynam.out","dave_cexcess.out")
group6 <- c( "dave_dalpha_leaf.out","dave_dalpha_root.out","dave_dalpha_sap.out", 
             "dave_dalpha_repr.out", "dave_dalpha_storage.out")
group7 <- c("dave_alpha_leaf_limit.out", "dave_alpha_root_limit.out", "dave_alpha_sap_limit.out", 
            "dave_alpha_repr_limit.out","dave_alpha_storage_limit.out")
variable_to_plot <- c(group1, group2, group3, group4, group5, group6, group7)
variable_to_plot <- c(group5, group6, group7)

out <- list()
for (var in variable_to_plot){
  tempdf <- read.table(paste0(basepath,"/",site,"/",version,"/",var), header=T)
  tempdf <- add_daysmonthsseasonsV2(tempdf)
  
  if (var == variable_to_plot[1]) {
    # save the first file name
    vars_to_plot <- var
    # save date vector
    date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
    season <- tempdf$season
    month <- tempdf$month
    # set length of first file to be the "standard" for analysis
    datalength <- dim(tempdf)[1]
    out[[var]] <- tempdf
  }
  # is there a pft column?
  ispft <- length(which(colnames(tempdf) == pft)) != 0
  # is there the same length as the first variable?
  issamelength <- dim(tempdf)[1] == datalength
  # create output file with only variables that have the same pft and have the same length
  if (issamelength && ispft && var != variable_to_plot[1]) {
    out[[var]] <- tempdf
    # add the file name to the vector of file names to be plotted
    vars_to_plot <- c(vars_to_plot, var)
  } 
  # save date vector
  if (var == variable_to_plot[1]) {
    date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
  }
}


subplot(
  plot_group_x(group1, out, date, pft),
  plot_group_x(group2, out, date, pft),
  plot_group_x(group3, out, date, pft),
  plot_group_x(group4, out, date, pft),
  plot_group_x(group5, out, date, pft),
  plot_group_x(group6, out, date, pft),
  plot_group_x(group7, out, date, pft),
  nrows = 7,
  shareX = TRUE,
  titleY = FALSE
) %>%
  layout(
    xaxis = list(title = "Date"),
    margin = list(t = 50)
    # showlegend = T
    )

#### MONTHLY BOXPLOTS

subplot(
  boxplot_group_x(group1, out, date, pft),
  boxplot_group_x(group2, out, date, pft),
  # plot_group_x(group3, out, date, pft),
  boxplot_group_x(group4, out, date, pft),
  boxplot_group_x(group5, out, date, pft),
  # plot_group_x(group6, out, date, pft),
  nrows = 4,
  shareX = TRUE,
  titleY = FALSE
) %>%
  layout(
    xaxis = list(title = "Date"),
    margin = list(t = 50)
    # showlegend = T
  )

# ############## OLD CODE
# # only dave annual outputs
# vars_dave_annual <- list.files(pattern = "dave_a") 
# vars_dave_daily  <- list.files(pattern = "dave_")
# # only the dave daily outputs
# vars_dave_daily <- vars_dave_daily[ !vars_dave_daily %in% vars_dave_annual] 
# # all the other variables (defaults?)
# vars_other <- vars_all[ !vars_all %in% vars_dave_daily]
# 
# vars_alphas <- c("dave_dalpha_leaf.out","dave_dalpha_root.out"
#                  ,"dave_dalpha_sap.out","dave_dalpha_repr.out")
# 
# vars_met <- list.files(pattern = "dave_met")
# 
# vars_allometry <- c("dave_crownarea.out", "dave_lai.out",
#                     "dave_height.out", "dave_diameter.out", 
#                     # "dave_sapwood_area.out", "dave_latosa.out",
#                     "dave_diameter_inc.out", "dave_height_inc.out", "dave_basalarea_inc.out",
#                     "dave_basalarea.out")
# 
# vars_soil_resources <- c("dave_wscal.out", "dave_nscal.out")
# vars_cflux <-c("dave_indiv_npp.out", "dave_indiv_gpp.out",
#                "dave_resp_maintenance.out","dave_resp_auto.out",
#                "dave_resp_growth.out","dave_gpp.out", "dave_npp.out")
# vars_cmass <- list.files(pattern = "dave_cmass")
# vars_nmass <- c(list.files(pattern = "dave_nmass"), "dave_ndemand.out")
# vars_cturnover <- list.files(pattern = "dave_dturnover")
# vars_cgrow <- list.files(pattern = "dave_cgrow")
# 
# 
# vars_C <- c(vars_cmass, vars_cgrow, vars_cturnover, vars_alphas, vars_allometry, 
#             vars_soil_resources, vars_cflux, vars_nmass)
# 
# vars_flux_lai <-c("dave_gpp.out", "dave_npp.out", "dave_lai.out")
# 
# vars_smass_pools <- c("dave_cmass_sap.out", "dave_cmass_leaf.out","dave_cmass_root.out",
#                       "dave_cmass_repr.out", "dave_cmass_heart.out", "dave_cmass_storage_dynam.out")
# vars_cmass <- c(vars_smass_pools, "dave_cmass_leaf_limit.out",       
#                 "dave_cmass_repr_limit.out","dave_cmass_repr.out","dave_cmass_root_limit.out", 
#                 "dave_cmass_sap_limit.out","dave_cmass_storage_max.out","dave_cmass_storage.out", 
#                 "dave_cmass.out",
#                 "dave_alpha_sap_limit.out", "dave_alpha_root_limit.out",
#                 "dave_alpha_leaf_limit.out",
#                 "dave_alpha_repr_limit.out")
# 
# vars_growth <- c("dave_lai.out", "dave_wscal.out", "dave_nscal.out",
#                  "dave_dalpha_leaf.out","dave_dalpha_root.out","dave_dalpha_sap.out",
#                  "dave_dalpha_repr.out","dave_cgrow.out", "dave_cgrow_leaf.out", 
#                  "dave_cgrow_repr.out","dave_cgrow_root.out","dave_cgrow_sap.out",
#                  "dave_diameter_inc.out","dave_height_inc.out", "dave_dturnover_leaf.out",
#                  "dave_dturnover_root.out","dave_dturnover_sap.out", "dave_basalarea_inc.out") 
# 
# vars_growth_coeff <- c("dave_ftemp.out", "dave_fstorage.out", "dave_fwater.out", "dave_cgrow_pot.out")
# # DAILY OR ANNUAL OUTPUTS?
# variable_to_plot <- c(vars_dave_daily, "dave_agd_g.out")
# # variable_to_plot <- vars_growth
# variable_to_plot <- c(vars_growth, vars_cflux)
# variable_to_plot <- c(vars_growth, 
#                       vars_cflux, 
#                       vars_cmass, 
#                       vars_growth_coeff,
#                       "dave_ltor.out",
#                       "dave_storage_dynam_14sum.out",
#                       "dave_meanleafage.out",
#                       "dave_latosa.out")
# variable_to_plot <- c(vars_growth,
#                       vars_cmass, 
#                       vars_growth_coeff,
#                       "dave_ltor.out",
#                       "dave_meanleafage.out")
# variable_to_plot <- c(vars_growth,
#                       vars_cflux,
#                       vars_cmass,
#                       "dave_ltor.out",
#                       "dave_meanleafage.out",
#                       "dave_latosa.out")
# variable_to_plot <-"dave_meanleafage.out"
# variable_to_plot <- vars_allometry
# variable_to_plot <- "dave_latosa.out"
# variable_to_plot <- vars_soil_resources
# variable_to_plot <- c("dave_lai.out")
# variable_to_plot <- "dave_density.out"
# variable_to_plot <- "dave_met_par.out"
# variable_to_plot <- "dave_met_vpd.out"
# variable_to_plot <- c("dave_cgrow_pot.out", "dave_ftemp.out", "dave_fwater.out", 
# "dave_fstorage.out", "dave_cmass_storage_dynam.out", 
# vars_growth, vars_allometry, vars_cflux)
# variable_to_plot <- c("dave_alpha_leaf_limit.out",
#                       "dave_alpha_root_limit.out",
#                       "dave_alpha_sap_limit.out",
#                       "dave_alpha_repr_limit.out")
# 
# out <- list()
# for (var in variable_to_plot){
#   tempdf <- read.table(paste0(basepath,"/",site,"/",version,"/",var), header=T)
#   tempdf <- add_daysmonthsseasonsV2(tempdf)
#   
#   if (var == variable_to_plot[1]) {
#     # save the first file name
#     vars_to_plot <- var
#     # save date vector
#     date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
#     season <- tempdf$season
#     month <- tempdf$month
#     # set length of first file to be the "standard" for analysis
#     datalength <- dim(tempdf)[1]
#     out[[var]] <- tempdf
#   }
#   # is there a pft column?
#   ispft <- length(which(colnames(tempdf) == pft)) != 0
#   # is there the same length as the first variable?
#   issamelength <- dim(tempdf)[1] == datalength
#   # create output file with only variables that have the same pft and have the same length
#   if (issamelength && ispft && var != variable_to_plot[1]) {
#     out[[var]] <- tempdf
#     # add the file name to the vector of file names to be plotted
#     vars_to_plot <- c(vars_to_plot, var)
#   } 
#   # save date vector
#   if (var == variable_to_plot[1]) {
#     date <- as.Date(paste0(out[[var]][,"Year"],"-",out[[var]][,"Day"]+1),format="%Y-%j")
#   }
# }
# 
# ########## ########## ########## 
# ######## TIMESERIES ########## 
# ########## ########## ########## 
# # plot one by one using plotly
# fig_timeseries <- plot_ly() 
# # %>% layout(yaxis = list(range=c(0,1.2)))
# for (var in vars_to_plot){
#   print(var)
#   # plot the variable
#   fig_timeseries <- fig_timeseries  %>% 
#     add_lines(x = date, y = out[[var]][,pft], name = var)
# }
# fig_timeseries 
# 
# 
# ######### ########## ########## 
# ########## MONTHLY ########## 
# ########## ########## ########## 
# # plot one by one using plotly
# fig_month <- plot_ly() 
# # %>% layout(yaxis = list(range=c(0,1.2)))
# for (var in vars_to_plot){
#   print(var)
#   # plot the variable
#   fig_month <- fig_month  %>% 
#     add_boxplot(x = month, y = out[[var]][,pft], name = var)
#   # add_lines(x = season, y = out[[var]][,pft], name = var)
# }
# fig_month



