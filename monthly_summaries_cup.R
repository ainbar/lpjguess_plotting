####################################################################################
# Creates monthly summaries for the valuebles across the 3 datasets (lai (BOM, CUP)
# flux data (CUP), DBH incrments (EucFACE))
####################################################################################

######## ######## ######## 
######## FUNCTIONS ######## 
######## ######## ######## 

# load some common functions
source('~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/common_functions.R')

######## ######## ######## ######## 
######## START CALCULATIONS ######## 
######## ######## ######## ######## 

# summarise DBH_inc/BA_inc by month
# remove NAs
temp_pos_DBH_inc_pos <- output_df[(!is.na(output_df$DBH_inc)|!is.na(output_df$BA_inc)),] 


# # leave only positive increments  (or 0)
# temp_pos_DBH_inc_pos <- temp_pos_DBH_inc_pos[
#   (temp_pos_DBH_inc_pos$DBH_inc >= 0 & temp_pos_DBH_inc_pos$BA_inc >= 0),] 



# # remove NA from month
# temp_pos_DBH_inc_pos <- temp_pos_DBH_inc_pos[!is.na(temp_pos_DBH_inc_pos$month),] 
# summarise by year and month
cup_DBH_BA_inc_byyear <- summaryBy(cbind(DBH_inc, BA_inc) ~ month+year,
                                data=temp_pos_DBH_inc_pos, FUN=median, keep.names=T)
cup_DBH_BA_inc_per_tree_byyear <- summaryBy(cbind(DBH_inc, BA_inc) ~ month+year+Tree,
                                   data=temp_pos_DBH_inc_pos, FUN=median, keep.names=T)

cup_DBH_BA_inc_byyear_season <- aggregate(temp_pos_DBH_inc_pos[,c("DBH_inc", "BA_inc")], 
                                  by=with(temp_pos_DBH_inc_pos, 
                                          list(month=month, 
                                               year=year, 
                                               Season=Season)), FUN=median)
cup_DBH_BA_inc_byyear_season$Season <-
  factor(cup_DBH_BA_inc_byyear_season$Season,
         levels = c("SUMMER", "AUTUMN","WINTER", "SPRING"))
# DBH_BA_inc_bymonth <- summaryBy(cbind(DBH_inc, BA_inc) ~ month,
#                                 data=cup_DBH_BA_inc_byyear, FUN=median, keep.names=T)
# NOTE: in this case, there is only monthly observations (I made sure of that in 
# "orginize_data.R") so I am taking the mean across the years per month
DBH_BA_inc_bymonth <- summaryBy(cbind(DBH_inc, BA_inc) ~ month,
                                    data=temp_pos_DBH_inc_pos, FUN=median, keep.names=T)

g1 <- ggplot(data=cup_DBH_BA_inc_byyear_season)+
  geom_boxplot(aes(x=as.factor(month), y=DBH_inc*(365.25/12), fill=Season))+
  theme_bw()+
  labs(x="Month", y="median DBH increment, cm/month")
g2 <- ggplot(data=cup_DBH_BA_inc_byyear_season)+
  geom_boxplot(aes(x=as.factor(Season), y=DBH_inc*(365.25/12)*3, fill=Season))+
  theme_bw()+
  labs(x="Season", y="median DBH increment, cm/season")

g1 / g2


####### BOM LAI ########
# summarise LAI by month
lai_cup_byyear <- summaryBy(cbind(lai, lai_inc) ~ year+month,
                             data=lai_cup, FUN=median, keep.names=T)
# lai_cup_bymonth <- summaryBy(lai ~ month,
#                              data=lai_cup_byyear, FUN=median, keep.names=T)
# NOTE: in this case, there is only monthly observations so I am taking 
# the mean across the years per month
lai_cup_bymonth <- summaryBy(cbind(lai, lai_inc) ~ month,
                             data=lai_cup, FUN=median, keep.names=T)
# # remove NA
# lai_cup_bymonth <- lai_cup_bymonth[!is.na(lai_cup_bymonth$month),] 


######## PAR LAI #########
# aggregate (take median) by month and year (due to variability)
lai_par_cup_byyear <- aggregate(lai_par[,c(2,3,7,8)], 
                         by=with(lai_par,list(year=year, month=month)), 
                         FUN=median, na.rm=T)
# set the season
lai_par_cup_byyear$Season <- ifelse(lai_par_cup_byyear$month %in% c(12,1,2),"SUMMER",
                             ifelse (lai_par_cup_byyear$month %in% c(3,4,5), "AUTUMN",
                                     ifelse (lai_par_cup_byyear$month %in% c(6,7,8), "WINTER", "SPRING")))
# the mean across the years per month
lai_par_cup_bymonth<- summaryBy(cbind(lai, lai_inc) ~ month,
                             data=lai_par_cup_byyear, FUN=median, keep.names=T)

######## MODIS LAI ##########
# aggregate (take median) by month and year (due to variability)
lai_modis_cup_byyear <- aggregate(lai_cup_modis500[,c(6,7)], 
                                by=with(lai_cup_modis500, list(year=year, month=month)), 
                                FUN=median, na.rm=T)
# the mean across the years per month
lai_modis_cup_bymonth<- summaryBy(cbind(lai, lai_inc) ~ month,
                                data=lai_modis_cup_byyear, FUN=median, keep.names=T)
# ggplot()+
#   geom_boxplot(data=lai_modis_cup_byyear, aes(x=as.factor(month), y=lai_inc))

# summarise flux data by month
# first summarise by year and month 
# NOTE: in this case, there are several (daily) measurements per month so I am taking 
# the median and not the mean
flux_data_cup_byyear <- summaryBy(cbind(precip, ET, GPP, NEP, ER) ~ year + month,
                                   data=flux_data_cup, FUN=sum, keep.names=T)

# take the median for Ta and Sws
flux_data_cup_byyear_TaSwsFsd <- summaryBy(cbind(Ta, Sws, Fsd) ~ year + month,
                                  data=flux_data_cup, FUN=median, keep.names=T)

flux_data_cup_byyear <- cbind(flux_data_cup_byyear, flux_data_cup_byyear_TaSwsFsd)
head(flux_data_cup_byyear)
# and then the result df by month
# NOTE: ... and then I take the median (across years) of the meidians (across months)
flux_data_cup_bymonth <- summaryBy(cbind(precip, ET, GPP, NEP, ER, Ta, Fsd, Sws) ~ month,
                                   data=flux_data_cup_byyear, FUN=median, keep.names=T)

# now soil moisture
# summarise by month and year (take the mean for each month)
EF_soil_moisture_byyear <- summaryBy(swc ~ year + month,
                                  data=EF_soil_moisture, FUN=median, keep.names=T)
# ...now take the median for the months across the years
EF_soil_moisture_bymonth <- summaryBy(swc ~ month,
                                     data=EF_soil_moisture_byyear, FUN=median, keep.names=T)

# create one dataframe to hold ALL the monthly summaries:
cup_bymonth_df <- cbind(lai_cup_bymonth, DBH_BA_inc_bymonth[c(2:3)], flux_data_cup_bymonth[c(2:9)], EF_soil_moisture_bymonth[2])
head(cup_bymonth_df)

# # other calculations
# cup_bymonth_df$gpp_lai <- cup_bymonth_df$GPP / cup_bymonth_df$lai
# cup_bymonth_df$gpp_lai_inc <- cup_bymonth_df$GPP / cup_bymonth_df$lai_inc
# cup_bymonth_df$gpp_ET <- cup_bymonth_df$GPP / cup_bymonth_df$ET
# cup_bymonth_df$ET_lai <- cup_bymonth_df$ET / cup_bymonth_df$lai
# cup_bymonth_df$BA_inc_lai_inc <-cup_bymonth_df$DBH_inc / cup_bymonth_df$lai_inc

# create a dataframe with relative values (ie relative to the maximum in each of the columns)
# cup_relative_montly_df <- create_relative_dataframe_of_monthly_values(cup_bymonth_df)
cup_relative_montly_df <- create_relative_dataframe_of_monthly_values_v1(cup_bymonth_df)

#### RESHAPE ACROSS MONTHS  #####
# reshape the relative dataframe such that it has 3 columns: month, variable and Value.
cup_relative_montly_df_rows <- reshape_cols_to_rows(cup_relative_montly_df)

#### RESHAPE ACROSS MONTHS AND YEARS #####

# Note for tomorrow: create yearly/monthly realtive !!!
# need to do that individially to every df.

# also, fix the temperature in cup_bymonth_df (it's summed up')


# calculate relative values
cup_relative_DBH_BA_inc_byyear <- create_relative_dataframe_of_monthly_values_across_years_V1(cup_DBH_BA_inc_byyear)
cup_relative_lai_cup_byyear <- create_relative_dataframe_of_monthly_values_across_years_V1(lai_cup_byyear)
cup_relative_flux_data_cup_byyear <- create_relative_dataframe_of_monthly_values_across_years_V1(flux_data_cup_byyear)
cup_relative_EF_soil_moisture_byyear <- create_relative_dataframe_of_monthly_values_across_years_V1(EF_soil_moisture_byyear)

# reshape DBH and BA increment across years and months
cup_DBH_BA_inc_byyear_reshaped <- reshape_cols_to_rows_year_month(cup_relative_DBH_BA_inc_byyear)
# reshape LAI across years and months
cup_lai_cup_byyear_reshaped <- reshape_cols_to_rows_year_month(cup_relative_lai_cup_byyear)
# reshape flux data across years and months
cup_flux_data_cup_byyear_reshaped <- reshape_cols_to_rows_year_month(cup_relative_flux_data_cup_byyear)
# reshape soil moisture across years and months
cup_EF_soil_moisture_byyear_reshaped <- reshape_cols_to_rows_year_month(cup_relative_EF_soil_moisture_byyear)
# bind all reshaped dfs
cup_relative_montly_df_rows_years_months <- rbind(cup_DBH_BA_inc_byyear_reshaped, 
                                              cup_lai_cup_byyear_reshaped, 
                                              cup_flux_data_cup_byyear_reshaped, 
                                              cup_EF_soil_moisture_byyear_reshaped)


