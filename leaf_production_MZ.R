setwd("~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/")
source("~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/functions_MZ.R")
library(mgcv)
library(ggplot2)
library(doBy)
library(dplyr)
library(tidyverse)
##### lai data
#### input
lai <- read.csv("data/lai_corrected_data_all_years.csv")
lai[1] <- NULL
lai$Date <- as.Date(lai$Date)

#### exclude days where fewer than 4 time steps were available;
#### exclude days where the standard deviation among the three PAR sensors in a ring was >0.03
lai <- subsetLAI(lai,
                 minnrHH=4,  
                 maxSD=0.03)

#### keep lai of waterlogging event; ring 6 was influenced by liara, remove; ring 5 was shut doen in Dec 2022, remove
lai <- subset(lai, Date < "2023-07-05")
lai <- subset(lai, !(Ring == "R6"))
lai <- subset(lai, !(Ring == "R5" & Date >= "2022-12-01"))


#### seperate the data to before and after the shut-down of ring 5
lai_before <- subset(lai, Date < "2022-12-01")
lai_after <- subset(lai, Date >= "2022-12-01" & Date < "2023-07-05")

##### litterfall
litring <- get_litring()
litter <- litterbyring(litring)

#### remove Ring 5, 6, 7, 8
litter <- subset(litter, !(Ring == "R5" & Date >= "2022-12-01"))
litter <- subset(litter, !(Ring %in% c("R6", "R7", "R8")))

#### seperate the data to before and after the shut-down of ring 5
litter_before <- subset(litter, Date < "2022-12-01")
litter_after <- subset(litter, Date >= "2022-12-01" & Date < "2023-07-05")

#### calculate change of lai
dLAIlitter_before <- make_dLAI_litter_before(lai_before, litter_before, kgam=80)
dLAIlitter_after <- make_dLAI_litter_after(lai_after, litter_after, kgam=20)
dLAIlitter <- rbind(dLAIlitter_before, dLAIlitter_after)

##### calculate leaf production
dLAIlitter$laprod <- with(dLAIlitter, 30.5 * (dLAI+dLAIlitter.mean)/ndays)
dLAIlitter$lit <- with(dLAIlitter, 30.5 * dLAIlitter.mean/ndays)
dLAIlitter$dLAIdt <- with(dLAIlitter, 30.5 * dLAI/ndays)

dLAIlitter$month <- month(dLAIlitter$Date)

