# Check biomass allometry at all Ozflux sites

# For sites with repeat censuses:
# 1. Calculate biomass with chosen allometries, generic allometry
# 2. Calculate growth -- do this by excluding trees from t1 if they're dead by t2

# Plot growth vs fluxes

#_______________________________________________________________________________
# Install packages:
require(ggplot2)
require(plotly)

#_______________________________________________________________________________
# Load data:
#------------
# Flux data
flux <- read.csv("data/total_ET_NEP_GPP_bw_surveys_v3.csv")

#------------
# Diameter data
# Cumberland Plain: 
# -- data cleaned except for 2023 census which has some missing tree measurements 
cup <- read.csv("data/Cumberland_Plain_diameter_height_biomass_data_manuallyCleaned20240318.csv")

# Great Western Woodlands: 
# -- not cleaned, individual trees can be matched in 2018-2023 but not 2012-2018 (new big multistemmed tree appears in 2018 and two big trees vanish)
# -- not sure how to properly account for dead trees
# -- 2023 data not yet in TERN portal and does not yet have biomass
# -- biomass estimates: know the paper but not the model, could not replicate Suzanne Prober's values for the earlier censuses but could get close
gww <- read.csv("data/GWW_diameter_height_biomass_basal_area_sapwood_data.csv")
gww_2023 <- read.csv("data/gww_2023_fromLachlanCharles.csv")

# Robson Creek: 
# -- data not cleaned
# -- looks reasonable, but there are some errors, might be good to try to clean?
robson <- read.csv("data/Robson_Creek_diameter_height_biomass_data.csv")

# Samford: 
# -- data not cleaned 
# -- looks reasonable, but there are some errors, unclear how to try to clean?
samford <- read.csv("data/Samford_diameter_height_biomass_data_mVFhel9.csv")

# Wombat State Forest: 
# -- data not cleaned and only partial match of individual trees (several new large trees appear in 2018, many trees with tags and no diameters)
wombat <- read.csv("data/Wombat_Stringybark_Eucalypt_diameter_height_biomass_EYriIKJ.csv")



#_______________________________________________________________________________
# Cumberland Plain

# Applying allometric equations to estimate biomass:
# from: Paul et al. 2013 http://dx.doi.org/10.1016/j.foreco.2013.08.054
# ------ Equation form: ln(y) = a + b * ln(x) + e
# ------ Linearised: exp(a + b * ln(DBH)) * CF
# Trees are either Eucalyptus or Melaleuca
# Use Eucalyptus equation for Eucalyptus, generic tree equation for Melaleuca:
# Eucalyptus 
a_euc <- -1.71
b_euc <- 2.21	
CF_euc <- 1.29
# Generic: Tree (<100) 
a_gen <- -1.82	
b_gen <- 2.27	
CF_gen <- 1.18
# Dead trees: stem reduction factor of 0.85 following Bennett et al. 2013 https://www.sciencedirect.com/science/article/pii/S0378112713004040?via%3Dihub

cup$biomass_kg <- NA
cup[which(cup$genus=="Eucalyptus"&cup$stemStatus=="Alive"),]$biomass_kg <-
  with(cup[which(cup$genus=="Eucalyptus"&cup$stemStatus=="Alive"),], 
       exp(a_euc + b_euc * log(stemDiameter_centimetres)) * CF_euc)

cup[which(cup$genus=="Eucalyptus"&cup$stemStatus=="Dead"),]$biomass_kg <- 
  with(cup[which(cup$genus=="Eucalyptus"&cup$stemStatus=="Dead"),], 
       exp(a_euc + b_euc * log(stemDiameter_centimetres)) * CF_euc * 0.85)

cup[which(cup$genus=="Melaleuca"&cup$stemStatus=="Alive"),]$biomass_kg <-
  with(cup[which(cup$genus=="Melaleuca"&cup$stemStatus=="Alive"),], 
       exp(a_gen + b_gen * log(stemDiameter_centimetres)) * CF_gen)

cup[which(cup$genus=="Melaleuca"&cup$stemStatus=="Dead"),]$biomass_kg <-
  with(cup[which(cup$genus=="Melaleuca"&cup$stemStatus=="Dead"),], 
       exp(a_gen + b_gen * log(stemDiameter_centimetres)) * CF_gen * 0.85)


# Matching trees across time:
cup$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", cup$startVisitDate)
cup_sub <- cup[,which(colnames(cup)%in%c("stemId", "genus", "CensusYear", 
                                         "biomass_kg", "stemStatus"))]

cup_sub2 <- reshape(cup_sub, idvar=c("stemId","genus", "stemStatus"), 
                    timevar="CensusYear", direction="wide")

cup_sub3 <- cup_sub2[which(cup_sub2$stemStatus=="Alive"),] # here any NA records mean tree is dead

# Calculating growth by interval, removing trees from t1 if dead by t2
# converting to tonnes per ha
cup_int15 <- (sum(cup_sub3$biomass_kg.2015, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2015),]$biomass_kg.2014, na.rm=T))/1000
cup_int16 <- (sum(cup_sub3$biomass_kg.2016, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2016),]$biomass_kg.2015, na.rm=T))/1000
cup_int17 <- (sum(cup_sub3$biomass_kg.2017, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2017),]$biomass_kg.2016, na.rm=T))/1000
cup_int18 <- (sum(cup_sub3$biomass_kg.2018, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2018),]$biomass_kg.2017, na.rm=T))/1000
cup_int19 <- (sum(cup_sub3$biomass_kg.2019, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2019),]$biomass_kg.2018, na.rm=T))/1000
cup_int20 <- (sum(cup_sub3$biomass_kg.2020, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2020),]$biomass_kg.2019, na.rm=T))/1000
cup_int21 <- (sum(cup_sub3$biomass_kg.2021, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2021),]$biomass_kg.2020, na.rm=T))/1000
cup_int23 <- (sum(cup_sub3$biomass_kg.2023, na.rm=T) - sum(cup_sub3[!is.na(cup_sub3$biomass_kg.2023),]$biomass_kg.2021, na.rm=T))/1000

# Putting together:
cup_summary <- data.frame(matrix(data=NA, nrow=8, ncol=4))
colnames(cup_summary) <- c("Site", "startDate", "endDate", "BiomassGrowth_tC.ha")
cup_summary$Site <- c("CumberlandPlain")
dates <- sort(as.Date(unique(cup$startVisitDate), format=c("%d/%m/%Y")))
cup_summary$startDate <- dates[1:(length(dates)-1)]
cup_summary$endDate <- dates[-1]
cup_summary$BiomassGrowth_tC.ha <- c(cup_int15, cup_int16, cup_int17, cup_int18, cup_int19, cup_int20, cup_int21, cup_int23)*0.5 # converting biomass to C assuming 0.5
# ASSAF: calculate the monthly growth (tC/ha/month) by dividing the increment by the
# number of days in the range (ie tC/ha/day), and  multiplying it by days per month
dayspermonth <- 365.25 / 12
cup_summary$BiomassGrowth_tC.ha.month <- dayspermonth * (cup_summary$BiomassGrowth_tC.ha / (as.numeric(cup_summary$endDate - cup_summary$startDate)))
#_______________________________________________________________________________
# Great western woodlands
# matching up trees is guess work: tree 9_1 *is maybe* tree 13_1 in the first census?
# Limit to the core 1 ha
gww2 <- gww[which(gww$plotId=="Great Western Woodlands, core1ha"),]

# Applying allometric equations to estimate biomass:
# from: Jonson J. H., Freudenberger D. 2011 (https://www.publish.csiro.au/bt/BT11018)	
# Exact equation unclear, could not replicate exact values calculated by Susanne Prober for the 2012 and 2018 censuses
# Recalculating for all using the Eucalypt Genera equation in Table 5:
a <- -0.936
b <- 2.162

gww_2023$biomass_kg <- exp(a + b * log(gww_2023$stemDiameter_centimetres))
gww2$biomass_kg <- exp(a + b * log(gww2$stemDiameter_centimetres))

# Checking Susanne Prober's original calculations vs new calculations:
plot(biomass_kg~aboveGroundBiomass_kilograms, data = gww2)
abline(0,1)
(gww2$biomass_kg-gww2$aboveGroundBiomass_kilograms)


# Matching trees across time:
gww2$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", gww2$startVisitDate)
gww_2023$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", gww_2023$startVisitDate)

gww_sub <- gww2[,which(colnames(gww2)%in%c("stemId", "CensusYear", 
                                         "biomass_kg", "stemStatus"))]

gww_2023_sub <- gww_2023[,which(colnames(gww_2023)%in%c("stemId","CensusYear", 
                                            "biomass_kg", "stemStatus"))]

gww_sub2 <- rbind(gww_sub,gww_2023_sub)
gww_sub3 <- gww_sub2[which(gww_sub2$stemStatus=="Alive"),]

# -- accounting for dead trees: assuming stemId 13_1 in 2012 = stemId 9_1 in 2018
# -- other trees vanish but new ones appear -- not sure how to deal with this
gww_int18 <- (sum(gww_sub3[which(gww_sub3$CensusYear==2018),]$biomass_kg, na.rm=T) - 
            sum(gww_sub3[which(gww_sub3$CensusYear==2012&gww_sub3$stemId!="NA_13_1"),]$biomass_kg, na.rm=T))/1000

gww_int23 <- (sum(gww_sub3[which(gww_sub3$CensusYear==2023),]$biomass_kg, na.rm=T) - 
            sum(gww_sub3[which(gww_sub3$CensusYear==2018),]$biomass_kg, na.rm=T))/1000


# Putting together:
gww_summary <- data.frame(matrix(data=NA, nrow=2, ncol=4))
colnames(gww_summary) <- c("Site", "startDate", "endDate", "BiomassGrowth_tC.ha")
gww_summary$Site <- c("GreatWesternWoodlands")
dates <- sort(as.Date(c(unique(gww2$startVisitDate), unique(gww_2023$startVisitDate)), format=c("%d/%m/%Y")))
gww_summary$startDate <- dates[1:(length(dates)-1)]
gww_summary$endDate <- dates[-1]
gww_summary$BiomassGrowth_tC.ha <- c(gww_int18, gww_int23)*0.5 # converting biomass to C assuming 0.5


#_______________________________________________________________________________
# Robson
# Limit to the core 1 ha
robson2 <- robson[which(robson$plotId=="Robson Creek, core1ha"),]

# Matching trees across time:
robson2$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", robson2$startVisitDate)

robson_sub <- robson2[,which(colnames(robson2)%in%c("stemId", "CensusYear", 
                                           "aboveGroundBiomass_kilograms", "stemStatus"))]

# -- accounting for dead trees: DOUBLE CHECK THIS WORKS AS EXPECT (i.e., excluding diameters of trees when they are dead)
robson_sub2 <- reshape(robson_sub, idvar=c("stemId", "stemStatus"), 
                    timevar="CensusYear", direction="wide")
robson_sub3 <- robson_sub2[which(robson_sub2$stemStatus=="Alive"),] # any NA records mean tree is dead (or otherwise not measured)

# Calculating growth by interval, removing trees from t1 if dead by t2
# converting to tonnes per ha
rob_int15 <- (sum(robson_sub3$aboveGroundBiomass_kilograms.2015, na.rm=T) - 
            sum(robson_sub3[!is.na(robson_sub3$aboveGroundBiomass_kilograms.2015),]$aboveGroundBiomass_kilograms.2010, na.rm=T))/1000
rob_int19 <- (sum(robson_sub3$aboveGroundBiomass_kilograms.2019, na.rm=T) - 
            sum(robson_sub3[!is.na(robson_sub3$aboveGroundBiomass_kilograms.2019),]$aboveGroundBiomass_kilograms.2015, na.rm=T))/1000
rob_int23 <- (sum(robson_sub3$aboveGroundBiomass_kilograms.2023, na.rm=T) - 
            sum(robson_sub3[!is.na(robson_sub3$aboveGroundBiomass_kilograms.2023),]$aboveGroundBiomass_kilograms.2019, na.rm=T))/1000

# Putting together:
robson_summary <- data.frame(matrix(data=NA, nrow=3, ncol=4))
colnames(robson_summary) <- c("Site", "startDate", "endDate", "BiomassGrowth_tC.ha")
robson_summary$Site <- c("RobsonCreek")
dates <- sort(as.Date(unique(robson2$startVisitDate), format=c("%d/%m/%Y")))
robson_summary$startDate <- dates[1:(length(dates)-1)]
robson_summary$endDate <- dates[-1]
robson_summary$BiomassGrowth_tC.ha <- c(rob_int15, rob_int19, rob_int23)*0.5 # converting biomass to C assuming 0.5


#_______________________________________________________________________________
# Samford

# Matching trees across time:
samford$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", samford$startVisitDate)

samford_sub <- samford[,which(colnames(samford)%in%c("stemId", "CensusYear", 
                                                    "aboveGroundBiomass_kilograms", "stemStatus"))]

# -- accounting for dead trees: DOUBLE CHECK THIS WORKS AS EXPECT (i.e., excluding diameters of trees when they are dead)
samford_sub2 <- reshape(samford_sub, idvar=c("stemId", "stemStatus"), 
                       timevar="CensusYear", direction="wide")
samford_sub3 <- samford_sub2[which(samford_sub2$stemStatus=="Alive"),] # any NA records mean tree is dead (or otherwise not measured)

# Calculating growth by interval, removing trees from t1 if dead by t2
# converting to tonnes per ha
sam_int17 <- (sum(samford_sub3$aboveGroundBiomass_kilograms.2017, na.rm=T) - 
            sum(samford_sub3[!is.na(samford_sub3$aboveGroundBiomass_kilograms.2017),]$aboveGroundBiomass_kilograms.2012, na.rm=T))/1000


# Putting together:
samford_summary <- data.frame(matrix(data=NA, nrow=1, ncol=4))
colnames(samford_summary) <- c("Site", "startDate", "endDate", "BiomassGrowth_tC.ha")
samford_summary$Site <- c("Samford")
dates <- sort(as.Date(unique(samford$startVisitDate), format=c("%d/%m/%Y")))
samford_summary$startDate <- dates[1:(length(dates)-1)]
samford_summary$endDate <- dates[-1]
samford_summary$BiomassGrowth_tC.ha <- c(sam_int17)*0.5 # converting biomass to C assuming 0.5


#_______________________________________________________________________________
# Wombat

# Matching trees across time: NOT REALLY SURE HOW WELL THIS WORKS
wombat$CensusYear <- sub(".*[^0-9](\\d+)$", "\\1", wombat$startVisitDate)

wombat_sub <- wombat[,which(colnames(wombat)%in%c("stemId", "CensusYear", 
                                                     "aboveGroundBiomass_kilograms", "stemStatus"))]

# -- accounting for dead trees: DOUBLE CHECK THIS WORKS AS EXPECT -- NOT SURE THIS WORKS FOR WOMBAT GIVEN TREE MATCHING?
wombat_sub2 <- reshape(wombat_sub, idvar=c("stemId", "stemStatus"), 
                        timevar="CensusYear", direction="wide")
wombat_sub3 <- wombat_sub2[which(wombat_sub2$stemStatus=="Alive"),] # any NA records mean tree is dead (or otherwise not measured)

# Calculating growth by interval, removing trees from t1 if dead by t2
# converting to tonnes per ha
wom_int18 <- (sum(wombat_sub3$aboveGroundBiomass_kilograms.2018, na.rm=T) - 
            sum(wombat_sub3[!is.na(wombat_sub3$aboveGroundBiomass_kilograms.2018),]$aboveGroundBiomass_kilograms.2014, na.rm=T))/1000

alt_wom_int18 <- (sum(wombat_sub3$aboveGroundBiomass_kilograms.2018, na.rm=T) - 
                sum(wombat_sub3$aboveGroundBiomass_kilograms.2014, na.rm=T))/1000


# Putting together:
wombat_summary <- data.frame(matrix(data=NA, nrow=1, ncol=4))
colnames(wombat_summary) <- c("Site", "startDate", "endDate", "BiomassGrowth_tC.ha")
wombat_summary$Site <- c("WombatStateForest")
dates <- sort(as.Date(unique(wombat$startVisitDate), format=c("%d/%m/%Y")))
wombat_summary$startDate <- dates[1:(length(dates)-1)]
wombat_summary$endDate <- dates[-1]
wombat_summary$BiomassGrowth_tC.ha <- c(wom_int18)*0.5 # converting biomass to C assuming 0.5


#_______________________________________________________________________________
# Units for fluxes
# -- NEP originally in gC/m2, want tC/ha
# -- GPP originally in gC/m2, want tC/ha
# -- ET originally kg/ha, want tC/ha
#### DOUBLE CHECK UNIT CONVERSION
flux$total_NEP_tC.ha <- flux$total_NEP/100
flux$total_GPP_tC.ha <- flux$total_GPP/100
flux$total_ET_t.ha <- flux$total_ET*10





#_______________________________________________________________________________
# Fluxes and diameter together
allsites_diam_summary <- rbind(cup_summary, gww_summary, robson_summary, samford_summary, wombat_summary)
allsites_diam_summary$matchcode <- paste0(allsites_diam_summary$Site, allsites_diam_summary$endDate)
flux$matchcode <- paste0(flux$site,flux$end.date)

allsites_summary <- merge(allsites_diam_summary, flux[,6:9], by="matchcode")


#_______________________________________________________________________________
# Plotting
par(mar=c(5, 4, 4, 6) + 0.1) # default: 5.1 4.1 4.1 2.1
with(allsites_summary[which(allsites_summary$Site=="CumberlandPlain"),],
     plot(x=endDate, y=BiomassGrowth_tC.ha, type="o", bty="u", pch=16, 
          ylab="Carbon uptake (tC/ha)", xlab="Date",
          col="#0050FF", lwd=2, ylim=c(0,30)))

with(allsites_summary[which(allsites_summary$Site=="CumberlandPlain"),],
     lines(x=endDate, y=total_NEP_tC.ha, col="#F09900",  type="o", lwd=2, pch=16))

with(allsites_summary[which(allsites_summary$Site=="CumberlandPlain"),],
     lines(x=endDate, y=total_GPP_tC.ha, col="#D90000",  type="o", lwd=2, pch=16))

# scaleFactor <- max(allsites_summary_cup$BiomassGrowth_tC.ha) / max(allsites_summary_cup$total_ET_t.ha)
par(new=T)
with(allsites_summary[which(allsites_summary$Site=="CumberlandPlain"),],
     plot(x=endDate, y=total_ET_t.ha, col="#FFDD00", type="o", lwd=2, pch=16, 
          xlab="", ylab="", ylim=c(0,12000), axes=F))
axis(4, ylim=c(0,30000), las=1) #col.axis="#FFDD00",
mtext("Water loss (t/ha)", side=4, line=4) #col="#FFDD00",

legend("topleft", lwd=2, col=c("#0050FF","#F09900","#D90000","#FFDD00"), 
       legend=c("Biomass (aboveground C)", "Tower NEP", "Tower GPP", "Tower ET"), bty="n")
title(main="Cumberland Plain")
abline(h=0, lty=2)

par(mar=c(5, 4, 4, 6) + 0.1) # default: 5.1 4.1 4.1 2.1
with(allsites_summary[which(allsites_summary$Site=="GreatWesternWoodlands"),],
     plot(x=endDate, y=BiomassGrowth_tC.ha, type="o", bty="u", pch=16, 
          ylab="Carbon uptake (tC/ha)", xlab="Date",
          col="#0050FF", lwd=2, ylim=c(0,35)))

with(allsites_summary[which(allsites_summary$Site=="GreatWesternWoodlands"),],
     lines(x=endDate, y=total_NEP_tC.ha, col="#F09900",  type="o", lwd=2, pch=16))

with(allsites_summary[which(allsites_summary$Site=="GreatWesternWoodlands"),],
     lines(x=endDate, y=total_GPP_tC.ha, col="#D90000",  type="o", lwd=2, pch=16))

# scaleFactor <- max(allsites_summary_cup$BiomassGrowth_tC.ha) / max(allsites_summary_cup$total_ET_t.ha)
par(new=T)
with(allsites_summary[which(allsites_summary$Site=="GreatWesternWoodlands"),],
     plot(x=endDate, y=total_ET_t.ha, col="#FFDD00", type="o", lwd=2, pch=16, 
          xlab="", ylab="", ylim=c(0,20000), axes=F))
axis(4, ylim=c(0,30000), las=1) #col.axis="#FFDD00",
mtext("Water loss (t/ha)", side=4, line=4) #col="#FFDD00",

legend("topleft", lwd=2, col=c("#0050FF","#F09900","#D90000","#FFDD00"), 
       legend=c("Biomass (aboveground C)", "Tower NEP", "Tower GPP", "Tower ET"), bty="n")
title(main="GreatWesternWoodlands")





par(mar=c(5, 4, 4, 6) + 0.1) # default: 5.1 4.1 4.1 2.1
with(allsites_summary[which(allsites_summary$Site=="RobsonCreek"),],
     plot(x=endDate, y=BiomassGrowth_tC.ha, type="o", bty="u", pch=16, 
          ylab="Carbon uptake (tC/ha)", xlab="Date",
          col="#0050FF", lwd=2, ylim=c(0,120)))

with(allsites_summary[which(allsites_summary$Site=="RobsonCreek"),],
     lines(x=endDate, y=total_NEP_tC.ha, col="#F09900",  type="o", lwd=2, pch=16))

with(allsites_summary[which(allsites_summary$Site=="RobsonCreek"),],
     lines(x=endDate, y=total_GPP_tC.ha, col="#D90000",  type="o", lwd=2, pch=16))

# scaleFactor <- max(allsites_summary_cup$BiomassGrowth_tC.ha) / max(allsites_summary_cup$total_ET_t.ha)
par(new=T)
with(allsites_summary[which(allsites_summary$Site=="RobsonCreek"),],
     plot(x=endDate, y=total_ET_t.ha, col="#FFDD00", type="o", lwd=2, pch=16, 
          xlab="", ylab="", ylim=c(0,50000), axes=F))
axis(4, ylim=c(0,30000), las=1) #col.axis="#FFDD00",
mtext("Water loss (t/ha)", side=4, line=4) #col="#FFDD00",

legend("topleft", lwd=2, col=c("#0050FF","#F09900","#D90000","#FFDD00"), 
       legend=c("Biomass (aboveground C)", "Tower NEP", "Tower GPP", "Tower ET"), bty="n")
title(main="RobsonCreek")


# par(mar=c(5, 4, 4, 6) + 0.1) # default: 5.1 4.1 4.1 2.1
# with(allsites_summary,
#      plot(x=endDate, y=BiomassGrowth_tC.ha, type="o", bty="u", pch=16, 
#           ylab="Carbon uptake (tC/ha)", xlab="Date",
#           col="#0050FF", lwd=2, ylim=c(0,30)))
# 
# with(allsites_summary,
#      lines(x=endDate, y=total_NEP_tC.ha, col="#F09900",  type="o", lwd=2, pch=16))
# 
# with(allsites_summary,
#      lines(x=endDate, y=total_GPP_tC.ha, col="#D90000",  type="o", lwd=2, pch=16))
# 
# par(new=T)
# with(allsites_summary,
#      plot(x=endDate, y=total_ET_t.ha, col="#FFDD00", type="o", lwd=2, pch=16, 
#           xlab="", ylab="", ylim=c(0,12000), axes=F))
# axis(4, ylim=c(0,30000), las=1) #col.axis="#FFDD00",
# mtext("Water loss (t/ha)", side=4, line=4) #col="#FFDD00",
# 
# legend("topleft", lwd=2, col=c("#0050FF","#F09900","#D90000","#FFDD00"), 
#        legend=c("Biomass (aboveground C)", "Tower NEP", "Tower GPP", "Tower ET"), bty="n")
# title(main="All sites")

# 
# # to plot with ggplot should probably reshape as long first
# allsites_summary_cup <- allsites_summary[which(allsites_summary$Site=="CumberlandPlain"),]
# scaleFactor <- max(allsites_summary_cup$BiomassGrowth_tC.ha) / max(allsites_summary_cup$total_ET_tC.ha)
# ggplot(allsites_summary_cup, aes(x=endDate)) +
#   geom_line(aes(y=BiomassGrowth_tC.ha), col="#0050FF") +
#   geom_point(aes(y=BiomassGrowth_tC.ha), col="#0050FF")+
#   geom_line(aes(y=total_NEP_tC.ha), col="#F09900") +
#   geom_point(aes(y=total_NEP_tC.ha), col="#F09900")+
#   geom_line(aes(y=total_GPP_tC.ha), col="#D90000") +
#   geom_point(aes(y=total_GPP_tC.ha), col="#D90000")+
#   geom_line(aes(y=total_ET_tC.ha*scaleFactor), col="#FFDD00") +
#   geom_point(aes(y=total_ET_tC.ha*scaleFactor), col="#FFDD00")+
#   scale_y_continuous(name="Carbon uptake (tC/ha)", sec.axis=sec_axis(~./scaleFactor, name="Water loss (t/ha)"))+
#   theme_classic()
# 
# 
# cup.plot <- ggplot(allsites_summary_cup, aes(x=endDate)) +
#   geom_line(aes(y=BiomassGrowth_tC.ha), col="#0050FF") +
#   geom_point(aes(y=BiomassGrowth_tC.ha), col="#0050FF")+
#   geom_line(aes(y=total_NEP_tC.ha), col="#F09900") +
#   geom_point(aes(y=total_NEP_tC.ha), col="#F09900")+
#   geom_line(aes(y=total_GPP_tC.ha), col="#D90000") +
#   geom_point(aes(y=total_GPP_tC.ha), col="#D90000")+
#   geom_line(aes(y=total_ET_tC.ha*scaleFactor), col="#FFDD00") +
#   geom_point(aes(y=total_ET_tC.ha*scaleFactor), col="#FFDD00")+
#   scale_y_continuous(name="Carbon uptake (tC/ha)", sec.axis=sec_axis(~./scaleFactor, name="Water loss (t/ha)"))+
#   theme_classic()
# 
# ggplotly(cup.plot)
