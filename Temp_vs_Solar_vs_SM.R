rm(list = ls())

library(zoo)
library(ggplot2)
library(ggthemes)

# function to calculate the rolling mean of a few variables.
addmovingaveragecol <- function(ndays, inDF){
  inDF$Ta_smooth <- zoo::rollmean(inDF$Ta, k = ndays, fill = NA)
  inDF$Fsd_smooth <- zoo::rollmean(inDF$Fsd, k = ndays, fill = NA)
  inDF$Sws_smooth <- zoo::rollmean(inDF$Sws, k = ndays, fill = NA)
  inDF$GPP_smooth <- zoo::rollmean(inDF$GPP, k = ndays, fill = NA)
  # turn the moving average of soil moisture to scale between 0 to 1 (where 1 is  
  #   the maximum soil moisture across the whole time period nad 0 is the minimum)
  min_Sws_smooth <- min(inDF$Sws_smooth, na.rm = T)
  inDF$Sws_smooth_rel <- (inDF$Sws_smooth - min_Sws_smooth)
  max_Sws <- max(inDF$Sws_smooth_rel, na.rm = T)
  inDF$Sws_smooth_rel <- inDF$Sws_smooth_rel / max_Sws
  return(inDF)
}

# calculate_relative_variable <- function(inDF) {
#   min_Sws_smooth <- min(inDF$Sws_smooth, na.rm = T)
#   inDF$Sws_smooth_rel <- (inDF$Sws_smooth - min_Sws_smooth)
#   max_Sws <- max(inDF$Sws_smooth_rel, na.rm = T)
#   inDF$Sws_smooth_rel <- inDF$Sws_smooth_rel / max_Sws
#   return(inDF)
#   }

# Function to calculate day length for a specific date
day_length <- function(latitude, date) {
  # Constants
  rad <- pi / 180
  
  # Helper function to calculate the declination of the sun
  solar_declination <- function(day_of_year) {
    return(23.44 * sin((360 / 365) * (day_of_year - 81) * rad))
  }
  
  # Extract the day of the year from the date
  day_of_year <- yday(date)
  
  # Calculate the solar declination for the given day
  declination <- solar_declination(day_of_year)
  
  # Calculate the hour angle
  cos_hour_angle <- -tan(latitude * rad) * tan(declination * rad)
  cos_hour_angle <- pmin(pmax(cos_hour_angle, -1), 1) # Ensure value is within valid range for acos
  hour_angle <- acos(cos_hour_angle) / rad
  
  # Calculate the day length in hours
  day_length_hours <- 2 * hour_angle / 15
  
  return(day_length_hours)
}

setseason <- function(inDF) {
  inDF$Season <- ifelse(inDF$month %in% c(12,1,2),"SUMMER",
                         ifelse (inDF$month %in% c(3,4,5), "AUTUMN",
                                 ifelse (inDF$month %in% c(6,7,8), "WINTER", "SPRING")))
  return(inDF)
}
# inputs:
ndays <- 31
lat_boyagin <- -32.477093
lat_cup <- -33.6152
lat_robson <- -17.1175
lat_wombat <- -37.4222


folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
load(paste0(folder_common,'LPJ_allocation_phenology/data/data_CumberlandPlain.RData'))
cup <- site_data
rm(site_data)
cup$month <- month(cup$date)
cup$year <- year(cup$date)
head(cup)
cup_var <- cup[,c("date", "month","year","Fsd", "Ta", "Sws", "GPP")]
cup_var <- addmovingaveragecol(ndays, cup_var)
cup_var$day_length <- day_length(lat_cup, cup_var$date)
cup_var$site <- "cup"
head(cup_var)

# tumba <- read.csv(paste0(folder_common,'LPJ_allocation_phenology/data/tumba_df.csv'), header=T)
# tumba$month <- month(tumba$time)
# tumba$year <- year(tumba$time)
# head(tumba)
# tumba_var <- tumba[,c("time","month","year","Fsd", "Tamean", "Sws")]
# colnames(tumba_var)[1] <- "date"
# colnames(tumba_var)[5] <- "Ta"
# tumba_var <- addmovingaveragecol(ndays, tumba_var)
# tumba_var$site <- "tumba"
# head(tumba_var)


folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
load(paste0(folder_common,'LPJ_allocation_phenology/data/data_Boyagin.RData'))
boyagin <- site_data
rm(site_data)
boyagin$month <- month(boyagin$date)
boyagin$year <- year(boyagin$date)
head(boyagin)
boyagin_var <- boyagin[,c("date", "month","year","Fsd", "Ta", "Sws", "GPP")]
boyagin_var <- addmovingaveragecol(ndays, boyagin_var)
boyagin_var$day_length <- day_length(lat_boyagin, boyagin_var$date)
boyagin_var$site <- "boyagin"

folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
load(paste0(folder_common,'LPJ_allocation_phenology/data/data_RobsonCreek.RData'))
rc <- site_data
rm(site_data)
rc$month <- month(rc$date)
rc$year <- year(rc$date)
head(rc)
rc_var <- rc[,c("date", "month","year","Fsd", "Ta", "Sws", "GPP")]
rc_var <- addmovingaveragecol(ndays, rc_var)
rc_var$day_length <- day_length(lat_robson, rc_var$date)
rc_var$site <- "robson"


folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
load(paste0(folder_common,'LPJ_allocation_phenology/data/data_WombatStateForest.RData'))
wombat <- site_data
rm(site_data)
wombat$month <- month(wombat$date)
wombat$year <- year(wombat$date)
head(wombat)
wombat_var <- wombat[,c("date", "month", "year", "Fsd", "Ta", "Sws", "GPP")]
wombat_var <- addmovingaveragecol(ndays, wombat_var)
wombat_var$day_length <- day_length(lat_wombat, wombat_var$date)
wombat_var$site <- "wombat"

sites <- rbind(cup_var, boyagin_var, wombat_var, rc_var)
head(sites)

# aggregate across all the years (mean daily)
# remove NAs first
sites_noNas <- sites[!is.na(sites$Sws_smooth_rel) &
                       !is.na(sites$GPP_smooth), ]
head(sites_noNas)

vars <- c("Ta_smooth", "Fsd_smooth", "Sws_smooth", "GPP_smooth", 
          "Sws_smooth_rel", "day_length")
sites_byyear <- aggregate(sites_noNas[, vars], 
                        by=with(sites_noNas, 
                                list(site=site,
                                     year=year,
                                     month=month)), 
                        FUN=median, na.rm=T)
head(sites_byyear)
sites_byyear$Fsd_per_hour <- sites_byyear$Fsd_smooth / 
  sites_byyear$day_length

sites_byyear <- setseason(sites_byyear)

vars0 <- c(vars, "Fsd_per_hour")
sites_bymonth <- aggregate(sites_byyear[, vars0], 
                        by=with(sites_byyear, 
                                list(site=site,
                                     month=month)), 
                        FUN=median, na.rm=T)


# differences between max and min
vars2 <- c("Ta_smooth", "Fsd_smooth", "Sws_smooth", "GPP_smooth", 
           "Sws_smooth_rel", "day_length","Fsd_per_hour")
fun <- function(x){
  return(max(x)-min(x))
}
width_variable <- aggregate(sites_bymonth[, vars2],
          by=with(sites_bymonth,
                  list(site=site)),
          FUN=fun)


#### PLOT

ggplot(data=sites)+
  geom_point(aes(x=Ta_smooth, y=Fsd_smooth, 
                 color=as.factor(month), size=Sws_smooth_rel))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_point(data=sites_byyear, aes(x=Ta_smooth, y=Fsd_smooth, 
                 color=as.factor(month), size=Sws_smooth_rel))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_point(data=sites_bymonth, aes(x=Ta_smooth, y=Fsd_smooth, 
                                    color=as.factor(month), size=Sws_smooth_rel))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_point(data=sites_bymonth, aes(x=Ta_smooth, y=Fsd_smooth, 
                                     color=as.factor(month), size=Fsd_per_hour))+
  theme_bw()+
  facet_wrap(.~site)


ggplot()+
  geom_point(data=sites_bymonth, aes(x=day_length, y=Fsd_smooth, 
                                     color=as.factor(month)))+
  theme_bw()+
  facet_wrap(.~site)

# explore Fsd data
Fsd_sites <- data.frame(matrix(nrow=12, ncol=5))
colnames(Fsd_sites) <- c("month", "Fsd_cup", "Fsd_wombat", 
                         "Fsd_robson", "Fsd_boyagin")
Fsd_sites$month <- c(1:12)
Fsd_sites$Fsd_cup <- sites_bymonth[sites_bymonth$site == 'cup',]$Fsd_smooth
Fsd_sites$Fsd_wombat <- sites_bymonth[sites_bymonth$site == 'wombat',]$Fsd_smooth
Fsd_sites$Fsd_robson <- sites_bymonth[sites_bymonth$site == 'robson',]$Fsd_smooth
Fsd_sites$Fsd_boyagin <- sites_bymonth[sites_bymonth$site == 'boyagin',]$Fsd_smooth
ggplot()+
  geom_point(data=Fsd_sites, aes(x=Fsd_cup, y=Fsd_boyagin, color=as.factor(month)))+
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="Red")
  

# explore day_length data
day_length_sites <- data.frame(matrix(nrow=12, ncol=5))
colnames(day_length_sites) <- c("month", "day_length_cup", "day_length_wombat", 
                         "day_length_robson", "day_length_boyagin")
day_length_sites$month <- c(1:12)
day_length_sites$day_length_cup <- sites_bymonth[sites_bymonth$site == 'cup',]$day_length
day_length_sites$day_length_wombat <- sites_bymonth[sites_bymonth$site == 'wombat',]$day_length
day_length_sites$day_length_robson <- sites_bymonth[sites_bymonth$site == 'robson',]$day_length
day_length_sites$day_length_boyagin <- sites_bymonth[sites_bymonth$site == 'boyagin',]$day_length
ggplot()+
  geom_point(data=day_length_sites, aes(x=day_length_cup, y=day_length_boyagin, color=as.factor(month)))+
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="Red")

# explore soil moisutre (Sws) data
Sws_sites <- data.frame(matrix(nrow=12, ncol=5))
colnames(Sws_sites) <- c("month", "Sws_cup", "Sws_wombat", 
                                "Sws_robson", "Sws_boyagin")
Sws_sites$month <- c(1:12)
Sws_sites$Sws_cup <- sites_bymonth[sites_bymonth$site == 'cup',]$Sws_smooth_rel
Sws_sites$Sws_wombat <- sites_bymonth[sites_bymonth$site == 'wombat',]$Sws_smooth_rel
Sws_sites$Sws_robson <- sites_bymonth[sites_bymonth$site == 'robson',]$Sws_smooth_rel
Sws_sites$Sws_boyagin <- sites_bymonth[sites_bymonth$site == 'boyagin',]$Sws_smooth_rel
ggplot()+
  geom_point(data=Sws_sites, aes(x=Sws_cup, y=Sws_boyagin, color=as.factor(month)))+
  theme_bw()+
  geom_abline (slope=1, linetype = "dashed", color="Red")+
  ylim(0,1)+ xlim(0,1)




ggplot()+
  geom_boxplot(data=sites, aes(x=as.factor(month), y=day_length, 
                                     color=as.factor(month)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites, aes(x=as.factor(month), y=Ta, 
                               color=as.factor(month)))+
  theme_bw()+
  facet_wrap(.~site)

sites_byyear$Season <-
  factor(sites_byyear$Season,
         levels = c("SUMMER", "AUTUMN","WINTER", "SPRING"))


ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=Fsd_smooth, 
                                      color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=Fsd_per_hour, 
                               color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=GPP_smooth, 
                                      color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=Ta_smooth, 
                                      color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=Sws_smooth, 
                                      color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

ggplot()+
  geom_boxplot(data=sites_byyear, aes(x=as.factor(month), y=Sws_smooth_rel, 
                                      color=as.factor(Season)))+
  theme_bw()+
  facet_wrap(.~site)

library(ggridges)
ggplot()+
  geom_density_2d_filled(data=sites, aes(x=Ta, y=Fsd_smooth), show.legend = FALSE)+
  facet_wrap(.~site)+
  theme_bw()+
  coord_cartesian(expand = FALSE)+
  geom_point(data=sites_bymonth ,aes(x=Ta_smooth, y=Fsd_smooth, color=as.factor(month)))

ggplot(sites, aes(x = Ta_smooth, y = factor(month))) +
  geom_density_ridges(fill = "gray90") +
  labs(x = "Temperature (°C)", y = "Month")
