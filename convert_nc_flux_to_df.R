# this is how you extract one variabel from netCDF into a new net CDF
# cdo daymax -selvar,Fsd ../Tumbarumba_L6_20020107_20191231.nc tumba_maxFsd.nc
# cdo daymin -selvar,Fsd ../Tumbarumba_L6_20020107_20191231.nc tumba_minFsd.nc
# cdo daymean -selvar,Fsd ../Tumbarumba_L6_20020107_20191231.nc tumba_meanFsd.nc
# # Merge all the .nc files in the directoy into one .nc file
# cdo merge *.nc tumba_daily.nc

rm(list = ls())

# Load necessary libraries
library(ncdf4)
library(ggplot2)
library(CFtime)

# define files and folders
filename_tumba_flux <- "tumba_daily.nc"
filename_robson_flux <- "robson_daily.nc"
folder <- "/Users/30060406/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
subfolder <- "data/flux_raw/"
site_robson <- "robson/"
site_tumba <- "tumba/"
# set working dir
setwd(folder)


# load input nc files
nc_tumba <- nc_open(paste0(folder, subfolder, site_tumba, filename_tumba_flux))
nc_robson <- nc_open(paste0(folder, subfolder, site_robson, filename_robson_flux))
timevec_tumba <- ncvar_get( nc_tumba, attributes(nc_tumba$dim)$names[1])
timevec_robson <- ncvar_get( nc_robson, attributes(nc_robson$dim)$names[1])
cf_tumba <- CFtime("days since 1800-01-01", "gregorian", timevec_tumba)
cf_tumba <- as.Date(as_timestamp(cf_tumba))
cf_robson <- CFtime("days since 1800-01-01", "gregorian", timevec_robson)
cf_robson <- as.Date(as_timestamp(cf_robson))

# variable names
var_names_tumba <- attributes(nc_tumba$var)$names
var_names_robson <- attributes(nc_robson$var)$names
data.frame(var_names_tumba=var_names_tumba, var_names_robson=var_names_robson)

ER_tumba <- ncvar_get(nc_tumba, "ER_SOLO")
NEE_tumba <- ncvar_get(nc_tumba, "NEE_SOLO")
GPP_tumba <- ncvar_get(nc_tumba, "GPP_SOLO")
Fsd_tumba <- ncvar_get(nc_tumba, "Fsd")
ET_tumba <- ncvar_get(nc_tumba, "ET")
precip_tumba <- ncvar_get(nc_tumba, "Precip")
Sws_tumba <- ncvar_get(nc_tumba, "Sws")
Tamean_tumba <- ncvar_get(nc_tumba, "Ta")
Tamax_tumba <- ncvar_get(nc_tumba, "Ta_2")
Tamin_tumba <- ncvar_get(nc_tumba, "Ta_3")
RHmean_tumba <- ncvar_get(nc_tumba, "RH_2")
RHmax_tumba <- ncvar_get(nc_tumba, "RH")
RHmin_tumba <- ncvar_get(nc_tumba, "RH_3")
VPDmean_tumba <- ncvar_get(nc_tumba, "VPD")
VPDmax_tumba <- ncvar_get(nc_tumba, "VPD_2")
VPDmin_tumba <- ncvar_get(nc_tumba, "VPD_3")

ER_robson <- ncvar_get(nc_robson, "ER_SOLO")
NEE_robson <- ncvar_get(nc_robson, "NEE_SOLO")
GPP_robson <- ncvar_get(nc_robson, "GPP_SOLO")
Fsd_robson <- ncvar_get(nc_robson, "Fsd")
ET_robson <- ncvar_get(nc_robson, "ET")
precip_robson <- ncvar_get(nc_robson, "Precip")
Sws_robson <- ncvar_get(nc_robson, "Sws")
Tamean_robson <- ncvar_get(nc_robson, "Ta_2")
Tamax_robson <- ncvar_get(nc_robson, "Ta")
Tamin_robson <- ncvar_get(nc_robson, "Ta_3")
RHmean_robson <- ncvar_get(nc_robson, "RH_2")
RHmax_robson <- ncvar_get(nc_robson, "RH")
RHmin_robson <- ncvar_get(nc_robson, "RH_3")
VPDmean_robson <- ncvar_get(nc_robson, "VPD_2")
VPDmax_robson <- ncvar_get(nc_robson, "VPD")
VPDmin_robson <- ncvar_get(nc_robson, "VPD_3")

# # test
# a <- data.frame(date=cf_robson, mean=RHmean_robson, max=RHmax_robson, min=RHmin_robson)
# head(a)
# ggplot(data=a)+
#   geom_line(aes(x=date, y=min), color="blue")+
#   geom_line(aes(x=date, y=mean), color="green")+
#   geom_line(aes(x=date, y=max), color="red")



# convert variables from netCDF to df

# time vector

tumba_df <- data.frame(time=cf_tumba, ER=ER_tumba, NEE=NEE_tumba, GPP=GPP_tumba, 
                       ET=ET_tumba, precip=precip_tumba, 
                       Fsd=Fsd_tumba, Sws=Sws_tumba, Tamean=Tamean_tumba, 
                       Tamax=Tamax_tumba, Tamin=Tamin_tumba, 
                       RHmean=RHmean_tumba, RHmax=RHmax_tumba, 
                       RHmin=RHmin_tumba, VPDmean=VPDmean_tumba, 
                       VPDmax=VPDmax_tumba, VPDmin=VPDmin_tumba)

robson_df <- data.frame(time=cf_robson, ER=ER_robson, NEE=NEE_robson, GPP=GPP_robson, 
                       ET=ET_robson, precip=precip_robson, 
                       Fsd=Fsd_robson, Sws=Sws_robson, Tamean=Tamean_robson, 
                       Tamax=Tamax_robson, Tamin=Tamin_robson, 
                       RHmean=RHmean_robson, RHmax=RHmax_robson, 
                       RHmin=RHmin_robson, VPDmean=VPDmean_robson, 
                       VPDmax=VPDmax_robson, VPDmin=VPDmin_robson)

write.csv(tumba_df,paste0(folder, "data/", "tumba_df.csv"))
nc_close(nc_tumba)

write.csv(robson_df,paste0(folder, "data/", "robson_df.csv"))
nc_close(nc_robson)

