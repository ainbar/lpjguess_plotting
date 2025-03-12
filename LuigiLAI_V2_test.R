
rm(list = ls())

# Load necessary libraries
library(ncdf4)
library(ggplot2)
library(CFtime)

# define files and folders
filename_tumba <- "Tumbarumba.nc"
filename_robson <- "RobsonCreek.nc"
filename_cup <- "CumberlandPlain.nc"
filename_wombat <-  "WombatStateForest.nc"
folder <- "/Users/30060406/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
subfolder <- "data/luigi_laiV2/"
folder_total <- "tot/"
folder_overstory <- "per/"
folder_understory <- "rec/"

# set working dir
setwd(folder)


extract_lai <- function(folder, subfolder, filename) {
  
  # load input nc files
  nc_site_tot <- nc_open(paste0(folder, subfolder, folder_total, filename))
  nc_site_overstory <- nc_open(paste0(folder, subfolder, folder_overstory, filename))
  nc_site_understory <- nc_open(paste0(folder, subfolder, folder_understory, filename))
  
  timevec_site <- ncvar_get( nc_site_tot, attributes(nc_site_tot$dim)$names[1] )
  cf_site <- CFtime( "hours since 1900-01-01 00:00:00", "gregorian", timevec_site )
  cf_site <- as.Date( as_timestamp(cf_site) )
  
  Band1_site_tot <- ncvar_get(nc_site_tot, "Band1")
  Band1_site_overstory <- ncvar_get(nc_site_overstory, "Band1")
  Band1_site_understory <- ncvar_get(nc_site_understory, "Band1")
  
  nc_close(nc_site_tot)
  nc_close(nc_site_overstory)
  nc_close(nc_site_understory)
  
  tot <- data.frame(Date=cf_site, lai=Band1_site_tot, veg="total")
  overstory <- data.frame(Date=cf_site, lai=Band1_site_overstory, veg="overstory")
  understory <- data.frame(Date=cf_site, lai=Band1_site_understory, veg="understory")
  
  site <- rbind(tot,overstory,understory)
  
  return(site)
}

lai_tumba <- extract_lai(folder, subfolder, filename_tumba)
lai_cup <- extract_lai(folder, subfolder, filename_cup)
lai_robson <- extract_lai(folder, subfolder, filename_robson)
lai_wombat <- extract_lai(folder, subfolder, filename_wombat)


write.csv(lai_tumba, "data/lai_tumba_BOM_V2.csv")
write.csv(lai_cup, "data/lai_cup_BOM_V2.csv")
write.csv(lai_robson, "data/lai_robson_BOM_V2.csv")
write.csv(lai_wombat, "data/lai_wombat_BOM_V2.csv")


