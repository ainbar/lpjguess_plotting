library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(doBy)
library(TTR)
library(patchwork) # to plot several plots as panels

rm(list = ls())

dirr <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/"
source(paste0(dirr, 'common_functions.R'))
filee <- "data/CUP_LAI_20240517.csv"
cup_par <- read.csv(paste0(dirr, filee), header = T)
head(cup_par)
cup_par$Date <- as.Date(cup_par$Date)

par <- cup_par[,c(1, 11)]
colnames(par)[2] <- "lai"
par$sensor <- "par"
par <- calc_lai_increment(par)

par1 <- cup_par[,c(1, 12)]
colnames(par1)[2] <- "lai"
par1$sensor <- "par1"
par1 <- calc_lai_increment(par1)

par2 <- cup_par[,c(1, 13)]
colnames(par2)[2] <- "lai"
par2$sensor <- "par2"
par2 <- calc_lai_increment(par2)

par3 <- cup_par[,c(1, 14)]
colnames(par3)[2] <- "lai"
par3$sensor <- "par3"
par3 <- calc_lai_increment(par3)

parMel <- cup_par[,c(1, 17)]
colnames(parMel)[2] <- "lai"
parMel$sensor <- "parMel"
parMel <- calc_lai_increment(parMel)


cup_par_monthly <- rbind(par,par1,par2,par3,parMel)
cup_par_monthly$month <- month(cup_par_monthly$Date)
cup_par_monthly$year <- year(cup_par_monthly$Date)
head(cup_par_monthly)

######## PAR LAI #########
# aggregate (take median) by month and year (due to variability)
cup_par_monthly_byyear <- aggregate(cup_par_monthly[,c(2,4)], 
                                by=with(cup_par_monthly,
                                        list(sensor=sensor, year=year, month=month)), 
                                FUN=median, na.rm=T)
head(cup_par_monthly_byyear)
# the mean across the years per month
cup_par_monthly_bymonth<- aggregate(cup_par_monthly_byyear[,c(4,5)], 
                                    by=with(cup_par_monthly_byyear,
                                            list(sensor=sensor, month=month)), 
                                    FUN=median, na.rm=T)
head(cup_par_monthly_bymonth)
unique(cup_par_monthly_byyear$sensor)

cup_par_monthly_byyear$sensor <-
  factor(cup_par_monthly_byyear$sensor,
         levels = c("par1", "par2", "par3" ,"parMel", "par"))

ggplot(data=cup_par_monthly_byyear)+
  geom_boxplot(aes(x=as.factor(month), y=lai_inc, fill=sensor))+
  theme_bw()+
  ylim(-0.025, 0.025)+
  geom_hline(yintercept = 0, color = "blue",linetype = "dashed")+
  labs(x="Month",  y="LAI Increment, LAI/day")+
  facet_wrap(.~sensor)

ggplot(data=cup_par_monthly_byyear)+
  geom_boxplot(aes(x=as.factor(month), y=lai, fill=sensor))+
  theme_bw()+
  labs(x="Month",  y="LAI, m2/m2")+
  facet_wrap(.~sensor, , scales = "free_y")
