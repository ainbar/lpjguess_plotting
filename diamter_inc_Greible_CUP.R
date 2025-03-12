
# DATA INFORMATION AND CREDITS:
############
# ------> Griebel_mistletoe_dendrometer_processed.csv
# Processed and cleaned basal area increments (cm^2) from monthly dendrometer readings 
# of uninfected E. fibrosa, E. moluccana and Melaleuca decora and of E. fibrosa and 
# E. moluccana that were infected with mistletoe (A. miquelii). Data were published 
# in Griebel et al. Recovery from Severe Mistletoe Infection After Heat- and 
# Drought-Induced

# ------>  Griebel_mistletoe_inventories_processed.csv
# Processed and cleaned data from annual standing biomass inventories of the TERN core 
# hectare that were published in Griebel et al. Recovery from Severe Mistletoe 
# Infection After Heat- and Drought-Induced Mistletoe Death. Ecosystems (2021). 
# https://doi.org/10.1007/s10021-021-00635-7.

# ------>  Griebel_mistletoe_PAI_processed.csv
# This file contains processed and cleaned observations of plant area index in m2/m2
# from monthly digital canopy photography (processed following MacFarlane et al. 
#  2014). 5 images were collected at 3 plots within the TERN core hectare, and 
# presented are averages by plot +/- SE as published in Griebel et al. Recovery 
# from Severe Mistletoe

# ------>  Griebel_mistletoe_litter_processed.csv
# This file contains processed litter fall data in kg/ha/day from 9 traps within 
# the TERN core hectare that were sorted by species (eucalyptus, mistleote) and 
# others (bark, twigs, buds) and that were published in: Griebel et al. Recovery 
# from Severe Mistletoe Infection After Heat- and Drought-Induced Mistletoe Death. 
# Ecosystems (2021).
############

rm(list = ls())

########## ########## ########## 
########## FUNCTIONS ########## 
########## ########## ########## 

calc_increment_Griebel <- function (inDF) {
  # this function works on the dataframe "no_misslltoe_trees", which has specific 
  # structure. The input data is already in the form of BA increments (but for the 
  # period and not per day). DBH is only given in the beginning for each tree, so 
  # it is calculated  inside the function. 
  numberoftrees <- dim(inDF)[2]-1
  numberofdates <- dim(inDF)[1]-4
  # save the dates
  dates <- as.Date(inDF[5:dim(inDF)[1], 1], format = "%d/%m/%y")
  
  newcolnames <- c("Date", "treename", "species", "height", "BA_inc_total", 
                   "BA_inc", "DBH", "BA", "DBH_inc")
  new_tree_df <- data.frame(matrix(nrow=0,ncol=length(newcolnames)))
  colnames(new_tree_df) <- newcolnames
  
  for (i in 1:numberoftrees){
    # create a df with date column
    this_tree_df <- data.frame(Date = dates)
    this_tree_df$treename <- colnames(inDF)[i+1]
    # species column
    this_tree_df$species <- inDF[3,i+1]
    this_tree_df$height <- inDF[1,i+1]
    this_tree_df$BA_inc_total <- as.numeric(inDF[5:49,i+1])
    this_tree_df$BA_inc <- 0.0
    this_tree_df$DBH <- 0.0
    this_tree_df$BA <- 0.0
    this_tree_df$DBH_inc <- 0.0
    DBH_init <- as.numeric(inDF[2,i+1])
    this_tree_df$DBH[1] <- DBH_init
    BA_init <- (DBH_init / 2)^2 * pi
    this_tree_df$BA[1] <- BA_init
    for (t in 2:length(dates)){
      # go through the dataes (from the second measuremnt date) and calculate
      # BA (in cm^2), DBH (in cm), BA_inc (cm^2/day), DBH_inc (in cm/day)
      this_date <- this_tree_df$Date[t]
      previous_date <- this_tree_df$Date[t-1]
      daysgap <- as.numeric(this_date - previous_date)
      this_tree_df$BA_inc[t] <- this_tree_df$BA_inc_total[t] / daysgap
      this_tree_df$BA[t] <-  this_tree_df$BA[t-1] +  this_tree_df$BA_inc_total[t]
      this_tree_df$DBH[t] <- sqrt((4*this_tree_df$BA[t])/pi)
      this_tree_df$DBH_inc[t] <- (this_tree_df$DBH[t] - this_tree_df$DBH[t-1]) / daysgap
    }
    new_tree_df <- rbind(new_tree_df, this_tree_df)
  }
  
  new_tree_df$month <- month(new_tree_df$Date)
  new_tree_df$year <- year(new_tree_df$Date)
  return(new_tree_df)
}

# NOTE THAT THERE IS A FUNCTION LIKE THIS IN monthly_summaries_cup.R
create_relative_dataframe_of_monthly_values_v1 <- function(inDF){
  for (i in 2:length(colnames(inDF))) {
    this_col <- inDF[[i]]
    # NEW: this will scale between 0 (minimum of original data) to 1 (maximum 
    # of original data)
    min_this_col <- min(this_col)
    this_col <- this_col - min_this_col
    max_this_col <- max(this_col)
    inDF[[i]] <- this_col/max_this_col
  }
  return(inDF)
}

calculate_PAI_increment <- function(inDF) {
  # add an increment column
  inDF$PAI_inc <- 0
  for (t in 2:length(inDF$date)) {
    # calculate the number of days between measurements
    this_date <- inDF$date[t]
    previous_date <- inDF$date[t-1]
    daysgap <- as.numeric(this_date - previous_date)
    # calculate the dPAI/dt (where t = day)
    inDF$PAI_inc[t] <- (inDF$PAI[t] - inDF$PAI[t-1]) / daysgap
  }
  return(inDF)
}


######### ######### ######### 
######### LOAD DATA ######### 
######### ######### ######### 
folder_common <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/data/'

###### BASAL AREA INCREMETNS {PER TREE}
# load file
Grieble_dendro <- read.csv(paste0(folder_common, "Griebel_mistletoe_dendrometer_processed.csv"))
Grieble_PAI <- read.csv(paste0(folder_common, "Griebel_mistletoe_PAI_processed.csv"))
Griebel_litter <- read.csv(paste0(folder_common, "Griebel_mistletoe_litter_processed.csv"))

############## ############## ############## 
############## DENDROMETER DATA ############## 
############## ############## ############## 

no_misslltoe_trees <- Grieble_dendro[,-c(2:9)]

# # try with Misslttoe trees
# no_misslltoe_trees <- Grieble_dendro

new_tree_df <- calc_increment_Griebel(no_misslltoe_trees)

# summarise across the site
df_summary_site_step1 <- summaryBy(cbind(DBH_inc, BA_inc) ~ year + month,
                          data=new_tree_df, FUN=sum, keep.names=T)
df_summary_site_step2 <- summaryBy(cbind(DBH_inc, BA_inc) ~ year + month,
                                   data=df_summary_site_step1, FUN=median, keep.names=T)

# sum increments in case same month
new_df <- summaryBy(cbind(DBH_inc, BA_inc) ~ year + month + treename,
                    data=new_tree_df, FUN=sum, keep.names=T)

# remove negative diameter increment (leave the 0s)

new_tree_df_0positive <- new_df[new_df$DBH_inc >=0,]
head(new_tree_df)


# summarise by month
AG_dendto_cup_bymonth <- summaryBy(cbind(BA_inc, DBH_inc) ~ month,
                             data=new_df, FUN=median, keep.names=T)
# relative values
AG_dendto_cup_bymonth_rel <- create_relative_dataframe_of_monthly_values_v1(AG_dendto_cup_bymonth)

# 
# ggplot(data=new_tree_df_0positive)+
#   geom_line(aes(x=month, y=BA_inc, col=treename))

ggplot()+
  geom_boxplot(data=new_tree_df_0positive, aes(x=as.factor(month), y=BA_inc))+
  theme_bw()+
  title("Anne Griebel dendrometer data")

####### ######### ########
###### PAI PER SITE ######
####### ######### ########

# convert date to R format
Grieble_PAI$date <- as.Date(Grieble_PAI$date, format = "%d/%m/%y")
# add month and year columns
Grieble_PAI$month <- month(Grieble_PAI$date)
Grieble_PAI$year <- year(Grieble_PAI$date)
head(Grieble_PAI)

# summarise by date
Grieble_PAI_timeseries <- summaryBy(cbind(PAI, PAI_SEM) ~ date,
                                   data=Grieble_PAI, FUN=median, keep.names=T)
head(Grieble_PAI_timeseries)
Grieble_PAI_timeseries$month <- month(Grieble_PAI_timeseries$date)
Grieble_PAI_timeseries$year <- year(Grieble_PAI_timeseries$date)
# add an increment column
Grieble_PAI_timeseries <- calculate_PAI_increment(Grieble_PAI_timeseries)

# summarise by month
Grieble_PAI_byMonth <- summaryBy(cbind(PAI, PAI_SEM, PAI_inc) ~ month,
                                    data=Grieble_PAI_timeseries, FUN=median, keep.names=T)
# relative by month
Grieble_PAI_byMonth_rel <- create_relative_dataframe_of_monthly_values_v1(Grieble_PAI_byMonth)

# plot timeseries (PAI)
ggplot()+
  geom_line(data=Grieble_PAI_timeseries, aes(x=date, y=PAI))+
  theme_bw()+
  title("Anne Griebel PAI data")

# plot boxplot (PAI)
ggplot()+
  geom_boxplot(data=Grieble_PAI, aes(x=as.factor(month), y=PAI))+
  theme_bw()+
  title("Anne Griebel PAI data")

# plot timeseries (PAI increment)
ggplot()+
  geom_line(data=Grieble_PAI_timeseries, aes(x=date, y=PAI_inc))+
  theme_bw()+
  title("Anne Griebel PAI increment data")

# plot boxplot (PAI increment)
ggplot()+
  geom_boxplot(data=Grieble_PAI_timeseries, aes(x=as.factor(month), y=PAI_inc))+
  theme_bw()+
  title("Anne Griebel PAI increment data")

###### ###### ###### 
###### LITTER ###### 
###### ###### ###### 

# convert date to R format
# start of data collection for this timeperiod
Griebel_litter$start <- as.Date(Griebel_litter$start, format = "%d/%m/%y")
# end of data collection for this timeperiod ( **time of measurement** )
Griebel_litter$end <- as.Date(Griebel_litter$end, format = "%d/%m/%y")
# number of days of litter accumulation between measurements
Griebel_litter$ndays <- as.numeric(Griebel_litter$end - Griebel_litter$start)
# add month and yar columns
Griebel_litter$month <- month(Griebel_litter$end)
Griebel_litter$year <- year(Griebel_litter$end)
head(Griebel_litter)

#  NOTE: NOT NECESSARY. THE DATA ALREADY COMES IN kg/ha/day 
# # calculate: *** Litter accumulation rates (gr/m^2/day) ***
# Griebel_litter$lit_accum_rate <- Griebel_litter$leaves_total / Griebel_litter$ndays
# head(Griebel_litter)

# create a timeseries (median for all the dates axcross all traps)
Grieble_Litter_timeseries <- aggregate(Griebel_litter[,c(5:8)], by=with(Griebel_litter, list(end=end)), FUN=median, na.rm=T)
# summarise by month
Grieble_Litter_byMonth <- aggregate(Griebel_litter[,c(5:8)], by=with(Griebel_litter, list(month=month)), FUN=median, na.rm=T)
# relative by month
Grieble_Litter_byMonth_rel <- 
  create_relative_dataframe_of_monthly_values_v1(Grieble_Litter_byMonth)


# plot timeseries (Litter (total - balck; eucs - red accumulation rate)
ggplot()+
  geom_line(data=Grieble_Litter_timeseries, aes(x=end, y=leaves_total))+
  geom_line(data=Grieble_Litter_timeseries, aes(x=end, y=eucalyptus), color="red")+
  theme_bw()+
  title("Anne Griebel Litter data")+
  ylab('Litter accumulate rate, kg/ha/day')


# plot boxplot (PAI)
ggplot()+
  geom_boxplot(data=Grieble_Litter_timeseries, aes(x=as.factor(month(end)), y=leaves_total))+
  theme_bw()+
  title("Anne Griebel Litter data")+
  ylab('median daily Litter accumulate rate, kg/ha/day')



###################################################################

## ## ## ## ## ## 
## SPIDER PLOT ## 
## ## ## ## ## ## 


maxmin <- c(1, 0)
base <- data.frame("Jan" = maxmin, "Feb" = maxmin, "Mar" = maxmin, "Apr" = maxmin,
                   "May" = maxmin, "Jun" = maxmin, "Jul" = maxmin, "Aug" = maxmin, 
                   "Sep" = maxmin, "Oct" = maxmin, "Nov" = maxmin, "Dec" = maxmin)

Var <- 2 # BA INCREMENT
BA_inc_rel <- data.frame( "Jan" = AG_dendto_cup_bymonth_rel[1,Var],
                          "Feb" = AG_dendto_cup_bymonth_rel[2,Var],
                          "Mar" = AG_dendto_cup_bymonth_rel[3,Var],
                          "Apr" = AG_dendto_cup_bymonth_rel[4,Var],
                          "May" = AG_dendto_cup_bymonth_rel[5,Var],
                          "Jun" = AG_dendto_cup_bymonth_rel[6,Var],
                          "Jul" = AG_dendto_cup_bymonth_rel[7,Var],
                          "Aug" = AG_dendto_cup_bymonth_rel[8,Var],
                          "Sep" = AG_dendto_cup_bymonth_rel[9,Var],
                          "Oct" = AG_dendto_cup_bymonth_rel[10,Var],
                          "Nov" = AG_dendto_cup_bymonth_rel[11,Var],
                          "Dec" = AG_dendto_cup_bymonth_rel[12,Var])
Var <- 2 # PAI
PAI_rel <- data.frame( "Jan" = Grieble_PAI_byMonth_rel[1,Var],
                           "Feb" = Grieble_PAI_byMonth_rel[2,Var],
                           "Mar" = Grieble_PAI_byMonth_rel[3,Var],
                           "Apr" = Grieble_PAI_byMonth_rel[4,Var],
                           "May" = Grieble_PAI_byMonth_rel[5,Var],
                           "Jun" = Grieble_PAI_byMonth_rel[6,Var],
                           "Jul" = Grieble_PAI_byMonth_rel[7,Var],
                           "Aug" = Grieble_PAI_byMonth_rel[8,Var],
                           "Sep" = Grieble_PAI_byMonth_rel[9,Var],
                           "Oct" = Grieble_PAI_byMonth_rel[10,Var],
                           "Nov" = Grieble_PAI_byMonth_rel[11,Var],
                           "Dec" = Grieble_PAI_byMonth_rel[12,Var])
Var <- 4 # PAI increment
PAI_inc_rel <- data.frame( "Jan" = Grieble_PAI_byMonth_rel[1,Var],
                       "Feb" = Grieble_PAI_byMonth_rel[2,Var],
                       "Mar" = Grieble_PAI_byMonth_rel[3,Var],
                       "Apr" = Grieble_PAI_byMonth_rel[4,Var],
                       "May" = Grieble_PAI_byMonth_rel[5,Var],
                       "Jun" = Grieble_PAI_byMonth_rel[6,Var],
                       "Jul" = Grieble_PAI_byMonth_rel[7,Var],
                       "Aug" = Grieble_PAI_byMonth_rel[8,Var],
                       "Sep" = Grieble_PAI_byMonth_rel[9,Var],
                       "Oct" = Grieble_PAI_byMonth_rel[10,Var],
                       "Nov" = Grieble_PAI_byMonth_rel[11,Var],
                       "Dec" = Grieble_PAI_byMonth_rel[12,Var])
Var <- 5 # litter accumulation (relative)
Total_Lit_Accum_rel <- data.frame("Jan" = Grieble_Litter_byMonth_rel[1,Var],
                           "Feb" = Grieble_Litter_byMonth_rel[2,Var],
                           "Mar" = Grieble_Litter_byMonth_rel[3,Var],
                           "Apr" = Grieble_Litter_byMonth_rel[4,Var],
                           "May" = Grieble_Litter_byMonth_rel[5,Var],
                           "Jun" = Grieble_Litter_byMonth_rel[6,Var],
                           "Jul" = Grieble_Litter_byMonth_rel[7,Var],
                           "Aug" = Grieble_Litter_byMonth_rel[8,Var],
                           "Sep" = Grieble_Litter_byMonth_rel[9,Var],
                           "Oct" = Grieble_Litter_byMonth_rel[10,Var],
                           "Nov" = Grieble_Litter_byMonth_rel[11,Var],
                           "Dec" = Grieble_Litter_byMonth_rel[12,Var])

Growth_Grieble_Responce <- rbind(base, PAI_rel, PAI_inc_rel, BA_inc_rel, Total_Lit_Accum_rel)
rownames(Growth_Grieble_Responce) <- c("max","min","PAI","PAI_inc", "DBH dendro inc", "Litter accumulation")


colors <- c("red","darkolivegreen4", "darkorange4","orange")
titles <- c("PAI (relative)", "PAI increment (relative)", "BA inc (relative)", "Litter accumulation (relative)")

# Reduce plot margin using par()
# Split the screen in 4 parts
op <- par(mar = c(1, 1, 1, 1))
par(mfrow = c(2, 2))

# Create the radar chart
for(i in 1:4){
  radarchart(Growth_Grieble_Responce[c(1,2,i+2),],
             title = titles[i],
             cglty = 1, pcol = colors[i], 
             pfcol = scales::alpha(colors[i], 0.2),
             cglcol = "gray",
             plwd = 2,        # Width of the line
             plty = 1)
}
par(op)

## 
Var <- 2 # litter accumulation (relative)
Euc_Lit_Accum_rel <- data.frame("Jan" = Grieble_Litter_byMonth_rel[1,Var],
                            "Feb" = Grieble_Litter_byMonth_rel[2,Var],
                            "Mar" = Grieble_Litter_byMonth_rel[3,Var],
                            "Apr" = Grieble_Litter_byMonth_rel[4,Var],
                            "May" = Grieble_Litter_byMonth_rel[5,Var],
                            "Jun" = Grieble_Litter_byMonth_rel[6,Var],
                            "Jul" = Grieble_Litter_byMonth_rel[7,Var],
                            "Aug" = Grieble_Litter_byMonth_rel[8,Var],
                            "Sep" = Grieble_Litter_byMonth_rel[9,Var],
                            "Oct" = Grieble_Litter_byMonth_rel[10,Var],
                            "Nov" = Grieble_Litter_byMonth_rel[11,Var],
                            "Dec" = Grieble_Litter_byMonth_rel[12,Var])
Var <- 3 # litter accumulation (relative)
mislt_Lit_Accum_rel <- data.frame("Jan" = Grieble_Litter_byMonth_rel[1,Var],
                                "Feb" = Grieble_Litter_byMonth_rel[2,Var],
                                "Mar" = Grieble_Litter_byMonth_rel[3,Var],
                                "Apr" = Grieble_Litter_byMonth_rel[4,Var],
                                "May" = Grieble_Litter_byMonth_rel[5,Var],
                                "Jun" = Grieble_Litter_byMonth_rel[6,Var],
                                "Jul" = Grieble_Litter_byMonth_rel[7,Var],
                                "Aug" = Grieble_Litter_byMonth_rel[8,Var],
                                "Sep" = Grieble_Litter_byMonth_rel[9,Var],
                                "Oct" = Grieble_Litter_byMonth_rel[10,Var],
                                "Nov" = Grieble_Litter_byMonth_rel[11,Var],
                                "Dec" = Grieble_Litter_byMonth_rel[12,Var])
Var <- 4 # litter accumulation (relative)
others_Lit_Accum_rel <- data.frame("Jan" = Grieble_Litter_byMonth_rel[1,Var],
                                  "Feb" = Grieble_Litter_byMonth_rel[2,Var],
                                  "Mar" = Grieble_Litter_byMonth_rel[3,Var],
                                  "Apr" = Grieble_Litter_byMonth_rel[4,Var],
                                  "May" = Grieble_Litter_byMonth_rel[5,Var],
                                  "Jun" = Grieble_Litter_byMonth_rel[6,Var],
                                  "Jul" = Grieble_Litter_byMonth_rel[7,Var],
                                  "Aug" = Grieble_Litter_byMonth_rel[8,Var],
                                  "Sep" = Grieble_Litter_byMonth_rel[9,Var],
                                  "Oct" = Grieble_Litter_byMonth_rel[10,Var],
                                  "Nov" = Grieble_Litter_byMonth_rel[11,Var],
                                  "Dec" = Grieble_Litter_byMonth_rel[12,Var])

Lit_Grieble_Responce <- rbind(base, Euc_Lit_Accum_rel, mislt_Lit_Accum_rel, others_Lit_Accum_rel)
rownames(Lit_Grieble_Responce) <- c("max","min","Euc_Lit_Accum_rel","mislt_Lit_Accum_rel", "Others_Lit_Accum_rel inc")

colors <- c("red","blue", "green", "black")

radarchart(Lit_Grieble_Responce,
           cglty = 1, pcol = colors,
           pfcol = scales::alpha(colors, 0.2),
           cglcol = "gray",
           plwd = 2,        # Width of the line
           plty = 1)
legend("topright",
       legend = paste( rownames(Lit_Grieble_Responce)[-c(1,2)]),
       bty = "n", pch = 20, col = colors,
       text.col = "grey25", pt.cex = 2)

# legend(x=1.5, y=1, legend = rownames(Lit_Grieble_Responce[-c(1,2),]), 
       # bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3))


# titles <- c("PAI (relative)", "PAI increment (relative)", "BA inc (relative)", "Litter accumulation (relative)")