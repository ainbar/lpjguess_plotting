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


basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"


site     <- "CapeTribulation"
site     <- "WombatStateForest"
#site     <- "Tumbarumba"
# site     <- "Calperum"
site     <- "CumberlandPlain"
# site <- "WallabyCreek"
# site <- "RobsonCreek"
# site <- "AliceSpringsMulga"

basline_folder <- paste0(basepath,'/',site,'/out')
setwd(basline_folder)
# setwd(paste0(basepath,'/',site,'/out/Wombat3'))

pft      <- "TeBE"

cutoff_year <- "2000-12-31"
cutoff_year_top <- "2100-01-01"
# add date variable to table
parseDate <- function (inDF) {
  inDF$Date <- as.Date(paste0(inDF$Year, "-", inDF$Day + 1), format = "%Y-%j")
  return(inDF)
}

# read the tables
read_output <- function(cutoff_year, cutoff_year_top, ofolder, ofilename, colnum, varname){
  # ofolder <- dir_sim_wo_phen
  # ofilename <- "dave_ftemp.out"
  # colnum <- 8
  # varname <- 'lai'
  df <- read.table(paste0(ofolder,"/",ofilename), header = T)
  # head(df)
  df <- parseDate(df)
  df <- df[, c( length(names(df)), colnum) ]
  names(df)[2] <- varname
  df <- df[df$Date > cutoff_year,]
  df <- df[df$Date < cutoff_year_top,]
  return(df)
}

# calculate lai increments
calc_lai_inc <- function(laiin) {
  laiin$lai_inc <- NA
  for (d in 2: length(laiin$Date)) {
    laiin$lai_inc[d] <-  laiin$lai[d] - laiin$lai[d-1]
  }
  return(laiin)
}

# prepare vertical bindings of each variable. Adds a Variable column and
#    changes the actual column name to "Value
prepare_4_rbind <- function(inDF,trtmnt) {
  inDF$Variable <- names(inDF)[2]
  inDF$Treatment <- trtmnt
  names(inDF)[2] <- "Value"
  return(inDF)
}

################

# read in the data from simulation without phen
folder_nophen <- "/CUP25_nophen_OriginalGrote_source_limited"
dir_sim_wo_phen <- paste0(basline_folder,folder_nophen)
# read in the data or simulation with phen
# folder_phen <- "/CUP16_set_alpha_leaf_pot_max_0_2"
folder_phen <- "/"
# folder_phen <- "/CUP17_newallocation_MetDataOnly"
dir_sim_w_phen  <- paste0(basline_folder,folder_phen)

############ read daily model outputs
####LAI
lai_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_lai.out", 8, 'lai')
lai_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_lai.out", 8, 'lai')
# calculate lai_inc
lai_inc_wo_phen <- calc_lai_inc(lai_wo_phen)
lai_inc_w_phen <- calc_lai_inc(lai_w_phen)
###BA INCREMENTS
ba_inc_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_basalarea_inc.out", 8, 'ba_inc')
ba_inc_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_basalarea_inc.out", 8, 'ba_inc')
####GPP, NPP
gpp_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_indiv_gpp.out", 8, 'gpp')
gpp_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_indiv_gpp.out", 8, 'gpp')
npp_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_indiv_npp.out", 8, 'npp')
npp_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_indiv_npp.out", 8, 'npp')
####C STORAGE DYNAMICS
cmass_storage_dynam_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_storage_dynam.out", 7, 'cmass_storage_dynam')
cmass_storage_dynam_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_storage_dynam.out", 7, 'cmass_storage_dynam')
#### ftemp, fwater, fstorage
ftemp_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_ftemp.out", 7, 'ftemp')
ftemp_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_ftemp.out", 7, 'ftemp')
fwater_wo_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_fwater.out", 7, 'fwater')
fwater_w_phen <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_fwater.out", 7, 'fwater')
#### MASS
cmass_leaf_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_leaf.out", 8, 'cmass_leaf_kgCm2')
cmass_leaf_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_leaf.out", 8, 'cmass_leaf_kgCm2')
cmass_sap_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_sap.out", 8, 'cmass_sap_kgCm2')
cmass_sap_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_sap.out", 8, 'cmass_sap_kgCm2')
cmass_heart_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_heart.out", 8, 'cmass_heart_kgCm2')
cmass_heart_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_heart.out", 8, 'cmass_heart_kgCm2')
cmass_root_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_root.out", 8, 'cmass_root_kgCm2')
cmass_root_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_root.out", 8, 'cmass_root_kgCm2')
cmass_storage_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_storage.out", 8, 'cmass_storage_kgCm2')
cmass_storage_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_storage.out", 8, 'cmass_storage_kgCm2')
cmass_storage_max_wo_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_wo_phen, "dave_cmass_storage_max.out", 8, 'cmass_storage_max_kgCm2')
cmass_storage_max_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cmass_storage_max.out", 8, 'cmass_storage_max_kgCm2')
cstor_frac_w_phen <- data.frame(Date = cmass_storage_max_w_phen$Date, 
                                cstor_frac = cmass_storage_w_phen$cmass_storage_kgCm2/cmass_storage_max_w_phen$cmass_storage_max_kgCm2)
#### CGROW
cgrow_w_phen <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_cgrow.out", 8, 'cgrow_kgCm2')
#### MET
Temp <-read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_met_temp.out", 7, 'temp')
wscal5 <- read_output(cutoff_year, cutoff_year_top, dir_sim_w_phen, "dave_wscal_5l.out", 8, 'wscal5')
#################### 

# make them into one DF. One with cbind and one with rbind
comb_site_data_horiz_wo_phen <- cbind(ba_inc_wo_phen, lai_inc_wo_phen[3], 
                                      gpp_wo_phen[2], npp_wo_phen[2], 
                                      Temp[2], wscal5[2])

# LAI INC
head(lai_inc_wo_phen)
vlai_inc_wo_phen <- prepare_4_rbind(lai_inc_wo_phen[,c(1,3)], "no_phen")
vlai_inc_w_phen <- prepare_4_rbind(lai_inc_w_phen[,c(1,3)], "phen")
# BA INC
vba_inc_wo_phen <- prepare_4_rbind(ba_inc_wo_phen, "no_phen")
vba_inc_w_phen <- prepare_4_rbind(ba_inc_w_phen, "phen")
#GPP NPP
vgpp_wo_phen <- prepare_4_rbind(gpp_wo_phen, "no_phen")
vgpp_w_phen <- prepare_4_rbind(gpp_w_phen, "phen")
vnpp_wo_phen <- prepare_4_rbind(npp_wo_phen, "no_phen")
vnpp_w_phen <- prepare_4_rbind(npp_w_phen, "phen")
# TEMP AND WSCAL5
vTemp <- prepare_4_rbind(Temp, "phen")
vwscal5 <- prepare_4_rbind(wscal5, "phen")
# STORAGE
vcmass_stor_w_phen <- prepare_4_rbind(cmass_storage_w_phen, "phen")
vcmass_stor_max_w_phen <- prepare_4_rbind(cmass_storage_max_w_phen, "phen")
vcstor_frac_w_phen <- prepare_4_rbind(cstor_frac_w_phen, "phen")
# CGROW
vcgrow_w_phen <- prepare_4_rbind(cgrow_w_phen, "phen")
  
###### CREATE A VERTICAL DF FOR THE COMBINED DATA

comb_site_data_ver <- rbind(
  vlai_inc_wo_phen, vlai_inc_w_phen, vba_inc_wo_phen, vba_inc_w_phen,
  vgpp_wo_phen, vgpp_w_phen, vnpp_wo_phen, vnpp_w_phen
)
head(comb_site_data_ver)
comb_site_data_ver$month <- month(comb_site_data_ver$Date)
comb_site_data_ver$year <- year(comb_site_data_ver$Date)

# for hypothesis/assumption plot with phenology
comb_data_assumption <- rbind(vTemp, vwscal5, vgpp_w_phen, vnpp_wo_phen,
                              vcstor_frac_w_phen, vcgrow_w_phen)


# Summaries by year and month
comb_sites_monthly_byyear <- aggregate(comb_site_data_ver[,c("Value")], 
                                       by=with(comb_site_data_ver, 
                                               list(month=month, 
                                                    year=year, 
                                                    variable=Variable,
                                                    Treatment=Treatment)),
                                       FUN=sum, na.rm=T )
names(comb_sites_monthly_byyear)[5] <- "Value"

# Change units in BA incements to cm2/tree
comb_sites_monthly_byyear[comb_sites_monthly_byyear$variable == "ba_inc",]$Value <-
  (comb_sites_monthly_byyear[comb_sites_monthly_byyear$variable == "ba_inc",]$Value * 10000)

head(comb_sites_monthly_byyear)

unique(comb_sites_monthly_byyear$variable)
comb_sites_monthly_byyear[comb_sites_monthly_byyear$variable == "ba_inc",]$variable <- "ba_inc_cm2"

# Summaries monthly across all years
comb_sites_bymonth <- aggregate(comb_sites_monthly_byyear[,"Value"], 
                                by=with(comb_sites_monthly_byyear, 
                                        list(month=month, 
                                             variable=variable,
                                             Treatment=Treatment)), 
                                FUN=median, na.rm=T)
names(comb_sites_bymonth)[4] <- "Value"
unique(comb_sites_bymonth$Treatment)

########################
################ PLOT 3
######################
no_phen = subset(comb_sites_bymonth, Treatment == "no_phen")
phen   = subset(comb_sites_bymonth, Treatment == "phen")

head(phen)

month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Create the plot
g1 <- ggplot(phen, aes(x = month)) +
  geom_line(data = subset(phen, variable == "gpp"), 
            aes(y = Value, color = "gpp"), linewidth = 1) +
  geom_line(data = subset(phen, variable == "npp"), 
            aes(y = Value, color = "npp"), linewidth = 1) +
  geom_point(data = subset(phen, variable == "gpp"), 
             aes(y = Value, color = "gpp"), size = 2) +
  geom_point(data = subset(phen, variable == "npp"), 
             aes(y = Value, color = "npp"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("gpp" = "blue", "npp" = "red"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("GPP, gC/m"^2*"/month"),
                     sec.axis = sec_axis(~ . , name = expression("GPP, gC/m"^2*"/month"))) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  )

# Create the plot
head(phen)
unique(phen$variable)
g2 <- ggplot(phen, aes(x = month)) +
  geom_line(data = subset(phen, variable == "ba_inc_cm2"), 
            aes(y = Value, color = "ba_inc_cm2"), linewidth = 1) +
  geom_line(data = subset(phen, variable == "lai_inc"), 
            aes(y = Value, color = "lai_inc"), linewidth = 1) +
  geom_point(data = subset(phen, variable == "ba_inc_cm2"), 
             aes(y = Value, color = "ba_inc_cm2"), size = 2) +
  geom_point(data = subset(phen, variable == "lai_inc"), 
             aes(y = Value, color = "lai_inc"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("ba_inc_cm2" = "green4", "lai_inc" = "black"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("BA increment, cm"^2*"/month"), 
                     sec.axis = sec_axis(~ . , name = expression("lai increment, m"^2*"/m"^2*"/month"))) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "green4"), # Primary y-axis color
    axis.title.y.right = element_text(color = "black"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "green4"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "black")   # Secondary y-axis ticks color
  )

g3 <- ggplot(no_phen, aes(x = month)) +
  geom_line(data = subset(no_phen, variable == "gpp"), 
            aes(y = Value, color = "gpp"), linewidth = 1) +
  geom_line(data = subset(no_phen, variable == "npp"), 
            aes(y = Value, color = "npp"), linewidth = 1) +
  geom_point(data = subset(no_phen, variable == "gpp"), 
             aes(y = Value, color = "gpp"), size = 2) +
  geom_point(data = subset(no_phen, variable == "npp"), 
             aes(y = Value, color = "npp"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("gpp" = "blue", "npp" = "red"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("GPP, gC/m"^2*"/month"),
                     sec.axis = sec_axis(~ . , name = expression("NPP, gC/m"^2*"/month"))) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  )

g2 <- ggplot(no_phen, aes(x = month)) +
  geom_line(data = subset(no_phen, variable == "ba_inc_cm2"), 
            aes(y = Value, color = "ba_inc_cm2"), linewidth = 1) +
  geom_line(data = subset(no_phen, variable == "lai_inc"), 
            aes(y = Value, color = "lai_inc"), linewidth = 1) +
  geom_point(data = subset(no_phen, variable == "ba_inc_cm2"), 
             aes(y = Value, color = "ba_inc_cm2"), size = 2) +
  geom_point(data = subset(no_phen, variable == "lai_inc"), 
             aes(y = Value, color = "lai_inc"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("ba_inc_cm2" = "green4", "lai_inc" = "black"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("BA increment, cm"^2*"/month"), 
                     sec.axis = sec_axis(~ . , name = expression("lai increment, m"^2*"/m"^2*"/month"))) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "green4"), # Primary y-axis color
    axis.title.y.right = element_text(color = "black"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "green4"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "black")   # Secondary y-axis ticks color
  )

(g1+g2)/(g3+g4)

g3+g4
(g3 + ylim(-40,167) )+(g4 + ylim(-0.5, 0.5))

# pdf("rplot.pdf") 
# g3+g4
# dev.off()
# 
# jpeg("rplot.jpg", width = 500, height = "200", res=600)
# g3+g4
# dev.off()

# ggsave(filename="rplot.jpg", plot=(g3+g4), width=8, height=3, units="in")
################

all_by_month <- read.csv("/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/all_byyear_bymonth.csv", header = T)

cup_obs_by_month <- subset(all_by_month, site == "CUP")



stats <- boxplot.stats(cup_obs_by_month$Value)$stats
y_limits <- range(stats)

# Create the boxplot
# ggplot(data, aes(x = Month, y = Value)) +
#   geom_boxplot(outlier.shape = NA) +  # Remove outliers from the plot
#   coord_cartesian(ylim = y_limits) +  # Adjust y-axis to non-outlier range
#   theme_minimal() +
#   labs(title = "Boxplot Without Outliers in Plot or Axis",
#        x = "Month",
#        y = "Value")
# 

# 
# 
# g11 <- ggplot()+
#   geom_boxplot(data = subset(cup_obs_by_month, Variable == "GPP"),
#                aes(x = as.factor(month), y = Value, fill = "GPP"), linewidth = 1)+
#   geom_boxplot(data = subset(cup_obs_by_month, Variable == "NEP"),
#                aes(x = as.factor(month), y = Value, fill = "NEP"), linewidth = 1)+
#   geom_line(data = subset(phen, variable == "gpp"),
#             aes(x = month, y = Value, color = "gpp"), linewidth = 1)+
#   geom_line(data = subset(phen, variable == "npp"),
#             aes(x = month, y = Value, color = "npp"), linewidth = 1)+
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   scale_fill_manual(values = c("GPP" = "blue", "NEP" = "red"),
#                      name = "Variable") +
#   scale_color_manual(values = c("gpp" = "blue", "npp" = "red"),
#                      name = "Variable") +
#   scale_y_continuous(name = expression("Productivity, gC/m"^2*"/month")) +
#   # scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
#   theme_minimal()
# 
# unique(cup_obs_by_month$Variable)
# ggplot() +
#   geom_boxplot(data = subset(cup_obs_by_month, Variable == "BAI_cm2_day"),
#                aes(x = as.factor(month), y = Value, fill = "BAI_cm2_day"), linewidth = 1)+
#   geom_line(data = subset(phen, variable == "ba_inc_cm2"),
#             aes(x = month, y = Value, color = "ba_inc_cm2"), linewidth = 1)+ 
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   scale_fill_manual(values = c("BAI_cm2_day" = "green4"),
#                     name = "Variable") +
#   scale_color_manual(values = c("ba_inc_cm2" = "green4"),
#                      name = "Variable") +
#   scale_y_continuous(name = expression("BA increment, m"^2*"/month")) +
#   # scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
#   theme_minimal()+ ylim(-5, 5)
# 
#   geom_boxplot(data = subset(cup_obs_by_month, Variable == "lai_inc"),
#                aes(x = as.factor(month), y = Value, fill = "lai_inc"), linewidth = 1)+
# 
#   geom_line(data = subset(phen, variable == "lai_inc"),
#             aes((x = month,y = Value, color = "lai_inc"), linewidth = 1) +
#   geom_point(data = subset(phen, variable == "ba_inc_cm2"),
#              aes((x = month,y = Value, color = "ba_inc_cm2"), size = 2) +
#   geom_point(data = subset(phen, variable == "lai_inc"),
#              aes((x = month,y = Value, color = "lai_inc"), size = 2) +
#   geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
#   scale_color_manual(values = c("ba_inc_cm2" = "green4", "lai_inc" = "black"),
#                      name = "Variable") +
#   scale_y_continuous(name = expression("BA increment, cm"^2*"/month"),
#                      sec.axis = sec_axis(~ . , name = expression("lai increment, m"^2*"/m"^2*"/month"))) +
#   scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
#   theme_minimal() +
#   theme(
#     legend.position = "none", # Remove legend
#     axis.title.y.left = element_text(color = "green4"), # Primary y-axis color
#     axis.title.y.right = element_text(color = "black"), # Secondary y-axis color
#     axis.text.y.left = element_text(color = "green4"),  # Primary y-axis ticks color
#     axis.text.y.right = element_text(color = "black")   # Secondary y-axis ticks color
#   )

########################
################ ALTERNATIVE PLOT 
######################

head(comb_data_assumption)
cutoff_early <- "2014-07-01"
cutoff_late <- "2016-01-01"
short <- comb_data_assumption[comb_data_assumption$Date >= cutoff_early,]
short <- short[short$Date <= cutoff_late,]
short[short$Variable == "cgrow_kgCm2",]$Value <- short[short$Variable == "cgrow_kgCm2",]$Value * 1000
short[short$Variable == "cgrow_kgCm2",]$Variable <- "cgrow_gCm2"
unique(short$Variable)

# short$Date <- as.Date(short$Date)

s1 <- ggplot() +
  geom_line(data = subset(short, Variable == "temp"), 
            aes(x = Date, y = Value, color = "temp"), linewidth = .7) +
  geom_line(data = subset(short, Variable == "wscal5"), 
            aes(x = Date, y = Value, color = "wscal5"), linewidth = .7) +
  geom_hline(yintercept = 0,  color = "black") +
  scale_color_manual(values = c("temp" = "brown", "wscal" = "gray3"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("Temperature, ˚c"),
                     sec.axis = sec_axis(~ . , name = expression("Relative Soil Moist."))) +
  # scale_x_continuous(name = "") +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "brown"), # Primary y-axis color
    axis.title.y.right = element_text(color = "gray3"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "brown"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "gray3")   # Secondary y-axis ticks color
  )

s2 <- ggplot() +
  geom_line(data = subset(short, Variable == "gpp"), 
            aes(x = Date, y = Value, color = "gpp"), linewidth = .7) +
  geom_line(data = subset(short, Variable == "npp"), 
            aes(x = Date, y = Value, color = "npp"), linewidth = .7) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("gpp" = "blue", "npp" = "red"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("GPP, gC/m"^2*"/month"),
                     sec.axis = sec_axis(~ . , name = expression("NPP, gC/m"^2*"/month"))) +
  # scale_x_continuous(name = "") +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  )
# unique(comb_data_assumption$Variable)
s3 <- ggplot() +
  geom_line(data = subset(short, Variable == "cstor_frac"), 
            aes(x = Date, y = Value, color = "cstor_frac"), linewidth = 0.7) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(values = c("cstor_frac" = "black"), name = "Variable") +
  scale_y_continuous(name = "C storage fraction") +
  # scale_x_continuous(name = "") +
  theme_minimal() +
  theme(
    legend.position = "",
  )

s4 <- ggplot() +
  geom_line(data = subset(short, Variable == "cgrow_gCm2"), 
            aes(x = Date, y = Value, color = "cgrow_gCm2"), linewidth = .7) +
  geom_hline(yintercept = 0, color = "black") +
  scale_color_manual(values = c("cgrow_gCm2" = "black"), name = "Variable") +
  scale_y_continuous(name = expression("C for growth, gC/m"^2*"")) +
  # scale_x_continuous(name = "") +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
  )

s1/s2/s3/s4
ggsave(filename="newmodel.jpg", plot=(s1/s2/s3/s4), width=4, height=10, units="in")

