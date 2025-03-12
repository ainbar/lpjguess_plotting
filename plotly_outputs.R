library(plotly)
library(ncdf4)

rm(list = ls())
## ------------------
## Simulations
## ------------------
## one PFT
#basepath <- "/Users/30044953/Documents/LPJ_GUESS/Code/lpjguess_dave_daily_grass_photosynthesis/benchmarks/ozflux"
basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"
#site     <- "Otway"
site     <- "CapeTribulation"
#site     <- "SturtPlains"
# site     <- "ClimGrass"
# site     <- "Tumbarumba"
#site     <- "DalyPasture"
#site     <- "Gingin"
#site     <- "YarramundiDegraded"
#site     <- "DryRiver" 
# site     <- "WombatStateForest"
#site     <- "Samford"
#site     <- "Tumbarumba"
# site     <- "Calperum"
site     <- "CumberlandPlain"
# site <- "WallabyCreek"
# site <- "RobsonCreek"
# site <- "AliceSpringsMulga"
# site <- "SilverPlains"
setwd(paste0(basepath,'/',site,'/out'))

pft      <- "TeBE"

vars_all <-list.files(path = ".") #
# only dave annual outputs
vars_dave_annual <- list.files(pattern = "dave_a") 
vars_dave_daily  <- list.files(pattern = "dave_")
vars_monthly  <- c("maet.out", "mgpp.out","mlai.out", "mnee.out", "mnpp.out", "nmass.out")
# only the dave daily outputs
vars_dave_daily <- vars_dave_daily[ !vars_dave_daily %in% vars_dave_annual] 
# all the other variables (defaults?)
vars_other <- vars_all[ !vars_all %in% vars_dave_daily]

vars_alphas <- c("dave_dalpha_leaf.out","dave_dalpha_root.out"
                 ,"dave_dalpha_sap.out","dave_dalpha_repr.out")
vars_gpp <- c("dave_")

vars_met <- list.files(pattern = "dave_met")

vars_allometry <- c("dave_crownarea.out", "dave_lai.out",
                    "dave_height.out", "dave_diameter.out", 
                    "dave_sapwood_area.out", "dave_latosa.out",
                    "dave_diameter_inc.out", "dave_height_inc.out")

vars_soil_resources <- c("dave_wscal.out", "dave_nscal.out")
vars_cflux <-c("dave_indiv_npp.out", "dave_indiv_assim_kg.out",
               "dave_indiv_resp_auto.out")
vars_cmass <- list.files(pattern = "dave_cmass")
vars_nmass <- c(list.files(pattern = "dave_nmass"), "dave_ndemand.out")
vars_cturnover <- list.files(pattern = "dave_dturnover")
vars_cgrow <- list.files(pattern = "dave_cgrow")
vars_storage <- c("dave_cmass_storage_max.out","dave_cmass_storage.out")
vars_C <- c(vars_cmass, vars_cgrow, vars_cturnover, vars_alphas, vars_allometry, 
            vars_soil_resources, vars_cflux, vars_nmass)


version <- "out"   ## default is 'out'

source("/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/functions.R")

vars_cmass <- c("dave_cmass_heart.out" , "dave_cmass_leaf_limit.out","dave_cmass_leaf.out",       
           "dave_cmass_repr_limit.out","dave_cmass_repr.out","dave_cmass_root_limit.out", 
           "dave_cmass_root.out" , "dave_cmass_sap_limit.out","dave_cmass_sap.out" ,       
           "dave_cmass_storage_max.out","dave_cmass_storage.out", "dave_cmass.out")


vars_growth <- c("dave_lai.out", "dave_wscal.out", "dave_nscal.out","dave_indiv_npp.out",
           "dave_indiv_assim_kg.out","dave_indiv_resp_auto.out","dave_dalpha_leaf.out",
           "dave_dalpha_root.out","dave_dalpha_sap.out","dave_dalpha_repr.out",
           "dave_cgrow_leaf.out", "dave_cgrow_repr.out","dave_cgrow_root.out","dave_cgrow_sap.out",
           "dave_diameter_inc.out","dave_height_inc.out", "dave_dturnover_leaf.out",
           "dave_dturnover_root.out","dave_dturnover_sap.out") 


# DAILY OR ANNUAL OUTPUTS?
variable_to_plot <- c(vars_dave_daily, "dave_agd_g.out")
# variable_to_plot <- vars_alphas
variable_to_plot <- vars_C
# variable_to_plot <- vars_allometry
# variable_to_plot <- vars_soil_resources
variable_to_plot <- vars_growth
# variable_to_plot <- vars_monthly
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


########## ########## ########## 
######## TIMESERIES ########## 
########## ########## ########## 
# plot one by one using plotly
fig_timeseries <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_timeseries <- fig_timeseries  %>% 
    add_lines(x = date, y = out[[var]][,pft], name = var)
}
fig_timeseries 


########## ########## 
######## SEASON ########## 
########## ########## 
# plot one by one using plotly
fig_season <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_season <- fig_season  %>% 
    add_boxplot(x = season, y = out[[var]][,pft], name = var)
    # add_lines(x = season, y = out[[var]][,pft], name = var)
}
fig_season


########## ########## ########## 
########## MONTHLY ########## 
########## ########## ########## 
# plot one by one using plotly
fig_month <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_month <- fig_month  %>% 
    add_boxplot(x = month, y = out[[var]][,pft], name = var)
  # add_lines(x = season, y = out[[var]][,pft], name = var)
}
fig_month

