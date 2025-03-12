# load some funcrions that are common to other R code
source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/common_functions.R')

# # WOMBAT SITE - CURRENTLY NOT USED
# # load WOMBAT site
# load('/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/data/data_WombatStateForest.RData')
# site_data_wombat <- site_data
# # cut the edges off the data. leaving 11 years of data
# site_data_wombat <- site_data_wombat[site_data_wombat$date < "2021-01-20", ]
folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
load(paste0(folder_common,'LPJ_allocation_phenology/data/data_CumberlandPlain.RData'))
flux_data_cup <- site_data
# cut the edges off the data. leaving 9 years of data
rm(site_data)
flux_data_cup <- addFLUXstuff(flux_data_cup)
# convert NEE to NEP
flux_data_cup$NEE <- -flux_data_cup$NEE
colnames(flux_data_cup)[6] <- "NEP"
head(flux_data_cup)

# load LAI

####### LAI FROM MODIS ########
# lai_wombat <- read.csv('/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/data/WombatStateForest_LAI.csv', header = T)
lai_cup_modis500 <- read.csv(paste0(folder_common, 'LPJ_allocation_phenology/data/CumberlandPlain_LAI.csv'), header = T)
# add columns: month, LAI anomaly
# lai_wombat <- addLAIstuff(lai_wombat)
lai_cup_modis500 <- addLAIstuff(lai_cup_modis500)
lai_cup_modis500 <- lai_cup_modis500[,c(4:5,7,9)]
lai_cup_modis500$year <- year(lai_cup_modis500$Date)
colnames(lai_cup_modis500)[2] <- "LAI"
# Smooth the dataset
lai_cup_modis500$lai <- SMA(lai_cup_modis500$LAI, n=14)
# change the name of the original LAI column to lai_obs
colnames(lai_cup_modis500)[2] <- "lai_obs"
# remove the NAs (as the smoothing average has NANs at the start of the timeseries)
lai_cup_modis500 <- lai_cup_modis500[!is.na(lai_cup_modis500$lai),]
lai_cup_modis500 <- calc_lai_increment(lai_cup_modis500)
head(lai_cup_modis500)
ggplot(data=lai_cup_modis500)+
  geom_boxplot(aes(x=as.factor(month), y=lai_inc))+
  ylim(-0.025, 0.025)
# write.csv(lai_cup_modis500, file="lai_cup_modis500_CUP.csv")
####### LAI FROM BOM ########
# load the new LAI product from Luigi (BOM)

lai_cup <- read.csv(paste0(folder_common, 'LPJ_allocation_phenology/data/lai_cup_BOM_V2.csv'), header = T)
lai_cup <- lai_cup[lai_cup$veg == "total",][,c(2,3)]
head(lai_cup)
lai_cup <- add_BOM_LAI_stff_V2(lai_cup)
lai_cup_month <- lai_cup[lai_cup$Date >= "2011-01-01",]
lai_cup_month <- lai_cup_month[lai_cup_month$Date < "2021-01-01",]
max(lai_cup_month$Date)
min(lai_cup_month$Date)
unique(lai_cup_month$month)

lai_BOM_monthly <- aggregate(lai_cup_month[,"lai"], 
                         by=with(lai_cup_month, 
                                 list(month=month, year=year)), 
                         FUN=median, na.rm=T)
head(lai_BOM_monthly)
lai_BOM_months <- aggregate(lai_BOM_monthly[,"x"], 
                        by=with(lai_BOM_monthly, 
                                list(month=month)), 
                        FUN=median, na.rm=T)
(lai_BOM_months)

####### LAI FROM PAR SENSORS ########
# load LAI from CUP itself (PAR sensor)
lai_par <- read.csv(paste0(folder_common, 'LPJ_allocation_phenology/data/CUP_LAI_20220915.csv'), header = T)
lai_par$Date <- as.Date(lai_par$Date)
lai_par$month <- month(lai_par$Date)
lai_par$year <- year(lai_par$Date)
head(lai_par)
temp <- lai_par
lai_par <- add_PAR_LAI_stff(lai_par)
head(lai_par)
ggplot(data=lai_par)+
  geom_boxplot(aes(x=as.factor(month), y=lai_inc*30))+
  ylim(-0.5, 0.5)

par_monthly <- aggregate(temp[,"LAI"], 
                         by=with(temp, 
                                 list(month=month, year=year)), 
                         FUN=median, na.rm=T)
head(par_monthly)
par_months <- aggregate(par_monthly[,"x"], 
                         by=with(par_monthly, 
                                 list(month=month)), 
                         FUN=median, na.rm=T)
par_monthly$date <- as.Date(paste0(par_monthly$year,"-",par_monthly$month,"-","01"))
par_monthly$lai_inc <- NA
for (t in 2:length(par_monthly$month)) {
  nummonths <- interval(par_monthly$date[t-1], par_monthly$date[t]) %/% months(1)
  par_monthly$lai_inc[t] <- (par_monthly$x[t] - par_monthly$x[t-1]) /nummonths
}

plot(lai_BOM_months$x, par_months$x)

ggplot(data=par_monthly)+
  geom_boxplot(aes(x=as.factor(month), y=))
# write.csv(par_monthly, file="lai_par_monthly_CUP.csv")
# write.csv(lai_par, file="lai_par_CUP.csv")

max(lai_par$Date)
# cut datasets according to date
mindate <- as.Date("2010-01-01")
maxdate <- as.Date("2022-07-01")
# clip flux data
flux_data_cup <- flux_data_cup[flux_data_cup$date >= "2014-01-01", ]
flux_data_cup <- flux_data_cup[flux_data_cup$date <= "2021-12-31", ]
flux_data_cup$NER <- flux_data_cup$GPP - flux_data_cup$NEP
# clip lai data
lai_cup <- lai_cup[lai_cup$Date >= mindate, ]
lai_cup <- lai_cup[lai_cup$Date <= maxdate, ]
# soil moisture data (EucFACE)
EF_SM_folder <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/EucFACE/Soil_moisture/"
EF_soil_moisture_raw <- read.csv(paste0(EF_SM_folder,"npsoilwater.csv"), header = T)
EF_soil_moisture_raw$Date <- as.Date(EF_soil_moisture_raw$Date)

#  mean soil moisture across all rings (by date)
EF_soil_moisture_raw_summarybyDate <- aggregate(swc~Date,EF_soil_moisture_raw,mean)
EF_soil_moisture_raw_summarybyDate$month <- month(EF_soil_moisture_raw_summarybyDate$Date)
EF_soil_moisture_raw_summarybyDate$year <- year(EF_soil_moisture_raw_summarybyDate$Date)
EF_soil_moisture_raw_summarybyDate$Season <- ifelse(EF_soil_moisture_raw_summarybyDate$month %in% c(12,1,2),"SUMMER",
                                      ifelse (EF_soil_moisture_raw_summarybyDate$month %in% c(3,4,5), "AUTUMN",
                                              ifelse (EF_soil_moisture_raw_summarybyDate$month %in% c(6,7,8), "WINTER", "SPRING")))
EF_soil_moisture_raw_summarybyDate <- 
  EF_soil_moisture_raw_summarybyDate[EF_soil_moisture_raw_summarybyDate$Date >= "2014-01-01", ] # exclude dates below
EF_soil_moisture_raw_summarybyDate <- 
  EF_soil_moisture_raw_summarybyDate[EF_soil_moisture_raw_summarybyDate$Date <= "2021-12-31", ] # exclude dates above
head(EF_soil_moisture_raw_summarybyDate)
# short name
EF_soil_moisture <- EF_soil_moisture_raw_summarybyDate


