# annual allocation fraction for trees
library(viridis)
library(dplyr)
library(ggplot2)
library(hrbrthemes)
library(plotly)

rm(list = ls())

# setwd("/Users/30060406/Dropbox/R_CODE/HIE/STSM/model_outputs/")

# load functions
source("/Users/30060406/Dropbox/R_CODE/HIE/STSM/model_outputs/useful_functions.R")

site<-"Warra"

# repo <- "/Users/30060406/git/lpjguess-dave"
repository <- "/Users/30060406/git/lpjguess_dave_assaf"
reppath <- paste0(repository, "/benchmarks/ozflux/", site, "/out/")

setwd(reppath)

# PFTs
pft <- "TRspE"
cols <- c("TRspE")
# in case we want to cut the data
earliest <- '1616-01-01' 
latest <-'2100-12-31'
# read in individual age
age <- read_output_new(reppath,"dave_indiv_age.out", earliest , latest)
age <- add_variable_as_col(age, "age", 9)
aage <- aggregate(value ~ Year + indiv + pft + variable, data = age, FUN = max)
# read in and make an annual sum of cgrow (total growth)
indiv_cgrow <- read_output_new(reppath,"dave_indiv_cgrow.out", earliest , latest)
indiv_cgrow <- add_variable_as_col(indiv_cgrow, "cgrow", 9)
aindiv_cgrow<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow, FUN = sum)
aindiv_cgrow$age <- aage$value
# read in and make an annual sum of cgrow_leaf
indiv_cgrow_leaf <- read_output_new(reppath,"dave_indiv_cgrow_leaf.out", earliest , latest)
indiv_cgrow_leaf <- add_variable_as_col(indiv_cgrow_leaf, "cgrow_leaf", 9)
aindiv_cgrow_leaf<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow_leaf, FUN = sum)
aindiv_cgrow_leaf$age <- aage$value
# read in and make an annual sum of cgrow_root
indiv_cgrow_root <- read_output_new(reppath,"dave_indiv_cgrow_root.out", earliest , latest)
indiv_cgrow_root <- add_variable_as_col(indiv_cgrow_root, "cgrow_root", 9)
aindiv_cgrow_root<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow_root, FUN = sum)
aindiv_cgrow_root$age <- aage$value
# read in and make an annual sum of cgrow_sap
indiv_cgrow_sap <- read_output_new(reppath,"dave_indiv_cgrow_sap.out", earliest , latest)
indiv_cgrow_sap <- add_variable_as_col(indiv_cgrow_sap, "cgrow_sap", 9)
aindiv_cgrow_sap<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow_sap, FUN = sum)
aindiv_cgrow_sap$age <- aage$value
# read in and make an annual sum of cgrow_repr
indiv_cgrow_repr <- read_output_new(reppath,"dave_indiv_cgrow_repr.out", earliest , latest)
indiv_cgrow_repr <- add_variable_as_col(indiv_cgrow_repr, "cgrow_repr", 9)
aindiv_cgrow_repr<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow_repr, FUN = sum)
aindiv_cgrow_repr$age <- aage$value
# read in and make an annual sum of cgrow_storage
indiv_cgrow_storage <- read_output_new(reppath,"dave_indiv_cgrow_storage.out", earliest , latest)
indiv_cgrow_storage <- add_variable_as_col(indiv_cgrow_storage, "cgrow_storage", 9)
aindiv_cgrow_storage<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_cgrow_storage, FUN = sum)
aindiv_cgrow_storage$age <- aage$value
head(indiv_cgrow_storage)
# read in and make an annual sum of gpp
indiv_gpp <- read_output_new(reppath,"dave_cohort_gpp.out", earliest , latest)
indiv_gpp <- add_variable_as_col(indiv_gpp, "gpp", 9)
aindiv_gpp<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_gpp, FUN = sum)
aindiv_gpp$age <- aage$value
# read in and make an annual sum of npp
indiv_npp <- read_output_new(reppath,"dave_cohort_npp.out", earliest , latest)
indiv_npp <- add_variable_as_col(indiv_npp, "npp", 9)
aindiv_npp<- aggregate(value ~ Year + indiv + pft + variable, data = indiv_npp, FUN = sum)
aindiv_npp$age <- aage$value

# bind annual dataframes
all_vars <- rbind(aindiv_gpp,
                  aindiv_npp,
                  aindiv_cgrow, aindiv_cgrow_leaf, 
                  aindiv_cgrow_root, 
                  aindiv_cgrow_sap, 
                  aindiv_cgrow_repr, 
                  aindiv_cgrow_storage)
# create a PFT_cohort column
all_vars$pft_cohort <- paste0(all_vars$pft, all_vars$indiv)


timeplotcohort <- ggplot(data=all_vars)+
  geom_line(aes(x=Year, y=value, col=pft_cohort))+
  theme_bw()+
  labs(x = "Year", y = "value")+
  facet_wrap(.~variable, ncol=1, scales="free_y")
ggplotly(timeplotcohort)

timeplot <- ggplot(data=all_vars)+
  geom_line(aes(x=Year, y=value, color=variable))+
  theme_bw()+
  labs(x = "Year", y = "value, kgC/m2")
ggplotly(timeplot)
# 
# all_vars %>%
#   ggplot( aes(x=Year, y=value, group=variable, fill=variable)) +
#   geom_area() +
#   scale_fill_viridis(discrete = TRUE) +
#   # theme(legend.position="none") +
#   # ggtitle("Popularity of American names in the previous 30 years") +
#   theme_ipsum() +
#   theme(
#     legend.position="none",
#     panel.spacing = unit(0.1, "lines"),
#     strip.text.x = element_text(size = 8)
#   ) +
#   facet_wrap(~variable)

ageplot <- ggplot(data=all_vars)+
  geom_point(aes(x=age, y=value, color=variable))+
  theme_bw()+
  labs(x = "age, yr", y = "value, kgC/m2")
ggplotly(ageplot)
