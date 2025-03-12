# adds columns to lai dataframe
addLAIstuff <- function(laiIN) {
  # setup date column
  laiIN$Date <- as.Date(laiIN$Date,format = "%d/%m/%y")
  # add a month column
  laiIN$month <- month(laiIN$Date)
  # add an lai-anomaly column
  laiIN$lai_anomaly <- laiIN$MYD15A2H_006_Lai_500m - mean(laiIN$MYD15A2H_006_Lai_500m)
  # set the season
  laiIN$Season <- ifelse(laiIN$month %in% c(12,1,2),"SUMMER",
                         ifelse (laiIN$month %in% c(3,4,5), "AUTUMN",
                                 ifelse (laiIN$month %in% c(6,7,8), "WINTER", "SPRING")))
  return(laiIN)
}


#####
# calculates monthly increments
calc_monthly_incs <- function(inDF) {
  # inDF <- Wombat_NHN_combined_timeseries_byYear
  inDF$RGR_month <- NA
  inDF$BAI_cm2_month <- NA
  inDF$dbh_inc_day <- NA
  for (i in 1:length(inDF$month)) {
    inDF$RGR_month[i] <- inDF$RGR_day[i] * daysinthismonth(inDF$month[i], inDF$year[i])
    inDF$BAI_cm2_month[i] <- inDF$BAI_cm2_day[i] *  daysinthismonth(inDF$month[i], inDF$year[i])
    inDF$dbh_inc_month <- inDF$dbh_inc_day[i] * daysinthismonth(inDF$month[i], inDF$year[i])
  }
  return(inDF)
}

#####

# calculate lai incremetns
calc_lai_increment <- function(inDF) {
  # add a column of lai_inc (rement) 
  inDF$lai_inc <- 0
  # calculate lai increment
  for (t in 2:length(inDF$Date)) {
    lai_previous <- inDF$lai[t-1]
    lai_now <- inDF$lai[t]
    timebetween <- as.numeric(inDF$Date[t] - inDF$Date[t-1])
    # lai increment in units of m^2/m^2/day
    inDF$lai_inc[t] <- (lai_now - lai_previous) / timebetween
  }
  return(inDF)
}

#####

# add columns to flux data datafframe
addFLUXstuff <- function(fluxIN) {
  # setup date column
  fluxIN$date <- as.Date(fluxIN$date,format = "%d/%m/%y")
  # add a month column
  fluxIN$month <- month(fluxIN$date)
  # add a year column
  fluxIN$year <- year(fluxIN$date)
  # set the season
  fluxIN$Season <- ifelse(fluxIN$month %in% c(12,1,2),"SUMMER",
                          ifelse (fluxIN$month %in% c(3,4,5), "AUTUMN",
                                  ifelse (fluxIN$month %in% c(6,7,8), "WINTER", "SPRING")))
  return(fluxIN)
}

#####

# adds to the BOM LAI dataframe (per site) a month and year columns
add_BOM_LAI_stff_V2 <- function(inDF){
  # inDF <- lai_in # DEBUGGING
  # keep only the date column and the data column (LAI) for the specific site
  outDF <- inDF
  outDF$Date <- as.Date(outDF$Date)
  # add a month column
  outDF$month <- month(outDF$Date)
  # add a year column
  outDF$year <- year(outDF$Date)
  # add an lai-anomaly column
  outDF$lai_anomaly <- outDF[[2]] - mean(outDF[[2]], na.rm = T)
  # set the season
  outDF$Season <- ifelse(outDF$month %in% c(12,1,2),"SUMMER",
                        ifelse (outDF$month %in% c(3,4,5), "AUTUMN",
                                ifelse (outDF$month %in% c(6,7,8), "WINTER", "SPRING")))
  outDF <- outDF[!is.na(outDF$month),]
  # calculate lai increment (m^2/m^2/day)
  outDF <- calc_lai_increment(outDF)
  return(outDF)
}

#####

# creates a dataframe with relative values, whereby the minimum value = 0 and the 
# maximum value = 1
# NOTE THAT THERE IS A FUNCTION LIKE THIS IN monthly_summaries_cup.R
# used to be in Monthly_summaries_cup.R
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

######

# creates a dataframe of monthly values with relative values (between 0 to 1)
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


######

# reshapes the dataframe to be in rows (for plotting in ggplot).
reshape_cols_to_rows <- function (inDF) {
  # Reshape the data frame
  reshaped_df <- inDF %>%
    pivot_longer(cols = -month, names_to = "Variable", values_to = "Value")
  return(reshaped_df)
}

######

# creates a dataframe with relative values (intended for the monthly variables)
create_relative_dataframe_of_monthly_values_across_years_V1 <- function(inDF){
  # create an outout dataframe
  outDF <- inDF[inDF$year == 2100,]
  # go through years and find the annual maximum and calculate the relative according to that
  yearlist <- unique(inDF$year)
  for (year_num in 1:length(yearlist)) {
    # cut off year
    this_year <- inDF[inDF$year == yearlist[year_num],]
    # go through columns and calcualate the relative for each one
    for (col_num in 3:length(colnames(this_year))) {
      # NEW: this will scale between 0 (minimum of original data) to 1 (maximum 
      # of original data)
      this_col <- this_year[[col_num]]
      min_this_col <- min(this_col)
      this_col <- this_col - min_this_col
      max_this_col <- max(this_col)
      this_year[[col_num]] <- this_col/max_this_col
    }
    # bind together into an outout df
    outDF <- rbind(outDF, this_year)
  }
  return(outDF)
}

#######

# this reshapes datafreames across years and months
reshape_cols_to_rows_year_month <- function (inDF) {
  # Reshape the data frame
  reshaped_df <- inDF %>%
    pivot_longer(cols = c(-month,-year), names_to = "Variable", values_to = "Value")
  return(reshaped_df)
}

#########

# this function does the same but assumes that the data in Anne's file are 
# ***cumulative*** BA increments, thus calculating the actual BAI
create_new_BAIdf_wombat_V1 <- function(inDF) {
  # list of tree names
  treenames <- colnames(inDF)[1:(length(colnames(inDF)) - 2)]
  # number of trees
  numberoftrees <- length(treenames)
  # list of dates
  dates <- as.Date(inDF[!is.na(inDF$date), ]$date, format="%d/%m/%Y")
  # number of dates 
  numberofdates <- length(dates)
  # create a new dataframe for the Wombat BAI data (source:Griebel)
  newcolnames <- c("date", "treeID", "species", "dbh_init", "canopy_class", 
                   "cumul_BA_inc_mm", "BA_inc_mm", "BA_inc_cm")
  new_tree_df <- data.frame(matrix(nrow=0, ncol=length(newcolnames)))
  colnames(new_tree_df) <- newcolnames
  # run through trees
  for (i in 1:numberoftrees) {
    this_tree_df <- data.frame(date = dates)
    this_tree_df$treeID <- treenames[i]
    this_tree_df$species <- inDF[2,i]
    this_tree_df$dbh_init <- inDF[3,i]
    this_tree_df$canopy_class <- inDF[1,i]
    this_tree_df$cumul_BA_inc_mm <- as.numeric(inDF[4:length(inDF[,1]),i])
    
    this_tree_df$BA_inc_mm <- 0
    this_tree_df$BA_inc_cm <- 0
    
    for (t in 2:numberofdates) {
      # go through the dates in each treeID, and calculate the tree BA increment 
      # (assuming that the raw data by Griebel is in cumulative).
      this_date <- this_tree_df$date[t]
      prev_date <- this_tree_df$date[t-1]
      # BA increment mm2/day
      this_tree_df$BA_inc_mm[t] <- 
        (this_tree_df$cumul_BA_inc_mm[t] - this_tree_df$cumul_BA_inc_mm[t-1]) /
        as.numeric(this_date - prev_date)
      # convert to cm2/day
      this_tree_df$BA_inc_cm[t] <- this_tree_df$BA_inc_mm[t] / 100
    }
    # remove NA
    this_tree_df <- this_tree_df[!is.na(this_tree_df$BA_inc_mm), ]
    # bind the new tree to the rest of the trees
    new_tree_df <- rbind(new_tree_df, this_tree_df)
  }
  # add month and year column
  new_tree_df$month <- month(new_tree_df$date)
  new_tree_df$year <- year(new_tree_df$date)
  # remove Nas (just in case)
  new_tree_df <- new_tree_df[!is.na(new_tree_df$BA_inc_mm), ]
  return(new_tree_df)
}

#######

create_new_PAIdf_wombat_V1 <- function(Wombat_PAVD_PAI_raw) {
  # remove the PAVD columns and stay with PAI
  Wombat_PAI <- Wombat_PAVD_PAI_raw[,-c(3:13)]
  colnames(Wombat_PAI) <- c("date", "pai")
  Wombat_PAI$date <- as.Date(Wombat_PAI$date, format="%d/%m/%Y")
  Wombat_PAI$month <- month(Wombat_PAI$date)
  Wombat_PAI$year <- year(Wombat_PAI$date)
  
  # calcualte PAI increment
  Wombat_PAI$pai_inc <- 0
  # make sure that the df is arranged in increasing date
  Wombat_PAI <- Wombat_PAI[order(Wombat_PAI$date),]
  # loop through dates and calcualte incremnt
  for (i in 2:length(Wombat_PAI$date)) {
    this_date <- Wombat_PAI$date[i]
    previous_date <- Wombat_PAI$date[i-1]
    # time difference
    timediff <- as.numeric(this_date - previous_date)
    # calcualte dPAI/dt
    Wombat_PAI$pai_inc[i] <- (Wombat_PAI$pai[i] - Wombat_PAI$pai[i-1]) / timediff
  }
  return(Wombat_PAI)
}

########

# creates a dataframe with relative values (intended for the monthly variables)
create_relative_dataframe_of_monthly_values <- function(inDF){
  for (i in 2:length(colnames(inDF))) {
    this_col <- inDF[[i]]
    max_this_col <- max(this_col)
    inDF[[i]] <- this_col/max_this_col
  }
  return(inDF)
}

# creates a dataframe with relative values (intended for the monthly variables)
create_relative_dataframe_of_monthly_values_across_years <- function(inDF){
  # create an outout dataframe
  outDF <- inDF[inDF$year == 2100,]
  # go through years and find the annual maximum and calculate the relative according to that
  yearlist <- unique(inDF$year)
  for (year_num in 1:length(yearlist)) {
    # cut off year
    this_year <- inDF[inDF$year == yearlist[year_num],]
    # go through columns and calcualate the relative for each one
    for (col_num in 3:length(colnames(this_year))) {
      this_col <- this_year[[col_num]]
      max_this_col <- max(this_col)
      this_year[[col_num]] <- this_col/max_this_col
    }
    # bind together into an outout df
    outDF <- rbind(outDF, this_year)
  }
  return(outDF)
}

##########

# creates a new df that has column names as month name from the datas tored in inDF
create_monthly_variable_spider <- function(inDF, Var) {
  outDF <- data.frame( "Jan" = inDF[1,Var],"Feb" = inDF[2,Var],"Mar" = inDF[3,Var],
                       "Apr" = inDF[4,Var],"May" = inDF[5,Var], "Jun" = inDF[6,Var],
                       "Jul" = inDF[7,Var],"Aug" = inDF[8,Var],"Sep" = inDF[9,Var],
                       "Oct" = inDF[10,Var],"Nov" = inDF[11,Var],"Dec" = inDF[12,Var])
  return(outDF)
}

###########

# defines the base of the dataframe for a spider plot. 
# example: response_dataframe <- rbind(definebase(), datacol)
definebase <- function(){
  maxmin <- c(1, 0)
  base <- data.frame("Jan" = maxmin, "Feb" = maxmin, "Mar" = maxmin, "Apr" = maxmin,
                     "May" = maxmin, "Jun" = maxmin, "Jul" = maxmin, "Aug" = maxmin, 
                     "Sep" = maxmin, "Oct" = maxmin, "Nov" = maxmin, "Dec" = maxmin)
  return(base)
}

########

# prepares the two databases of BA from Nina Hinko Najare
prepare_wombat_inc_NHN_raw_data <- function(inDF){
  # prepare an output df
  outDF <- data.frame(matrix(ncol=7,nrow=0))
  colnames(outDF) <- c("date", "Tree_ID", "Species", "days", "crown_class", 
                       "BAI_cm2", "dbh_init")
  treenames <- unique(inDF$Tree_ID)
  numberoftrees <- length(treenames)
  
  inDF$dbh_init <- 0
  inDF$BAI_cm2_day <- 0
  for (i in 1:numberoftrees) {
    # this tree data
    this_tree_data <- inDF[inDF$Tree_ID == treenames[i],]
    # save initial dbh in a column
    this_tree_data$dbh_init <- this_tree_data[1,]$DBH_prev_mm
    # remove the line where number of days is 0 
    this_tree_data <- this_tree_data[!this_tree_data$days == 0,]
    # calculate increment per day
    this_tree_data$BAI_cm2_day <- this_tree_data$BAI_cm2 / this_tree_data$days
    # remove the DBH_prev_mm column
    this_tree_data <- this_tree_data[,-6]
    # bind this tree's data to the rest of the trees 
    outDF <- rbind(outDF, this_tree_data)
  }
  
  return(outDF)
}

#########


# prepares the two databases of BA from Nina Hinko Najare. This time also 
# calculating relative growth rate per day

prepare_wombat_inc_NHN_raw_data_new <- function(inDF){
  # inDF <- Wombat_NHN_fluxsite # DEBUGGING
  names_df <- c("date", "Tree_ID", "Species", "days", "crown_class", 
                "BAI_cm2", "BAI_cm2_day", "dbh_inc_day", "RGR_day")
  # prepare an output df
  outDF <- data.frame(matrix(ncol=length(names_df), nrow=0))
  names(outDF) <- names_df
  
  treenames <- unique(inDF$Tree_ID)
  numberoftrees <- length(treenames)
  
  head(inDF)
  inDF$dbh_init <- 0
  inDF$BAI_cm2_day <- 0
  inDF$RGR_day <- 0
  for (i in 1:numberoftrees) {
    # this tree data
    this_tree_data <- inDF[inDF$Tree_ID == treenames[i],]
    # remove the line where number of days is 0 
    this_tree_data <- this_tree_data[!this_tree_data$days == 0,]
    # calculate BA increment per day
    this_tree_data$BAI_cm2_day <- this_tree_data$BAI_cm2 / this_tree_data$days

    # calculate DBH increment per day (cm/day)
    this_tree_data$dbh_inc_day <- (this_tree_data$DBH_curr_mm - this_tree_data$DBH_prev_mm) / 10 / this_tree_data$days
    
    # calculate RGR increment per day  (cm/day)
    this_tree_data$RGR_day <- ((this_tree_data$DBH_curr_mm - this_tree_data$DBH_prev_mm)  /
      this_tree_data$DBH_prev_mm) / this_tree_data$days
    
    # remove the DBH_prev_mm column
    this_tree_data <- this_tree_data[,-c(6,7)]
    
    # bind this tree's data to the rest of the trees 
    outDF <- rbind(outDF, this_tree_data)
  }
  
  return(outDF)
}


########
# return the number of days for a given year and month. Takes into account leap years
daysinthismonth <- function(inmonth, inyear) {
  if (inmonth == 1 || inmonth == 3|| inmonth == 5 | inmonth == 7 || inmonth ==8 || inmonth == 10 || inmonth == 12) {
    return(31)
  } else if (inmonth == 4 || inmonth == 6 || inmonth == 9 || inmonth == 11){
    return(30)
  } else if(inyear %% 4 == 0) {
    return(29)
  } else {
    return(28)
  }
}

#########

# adds a data columns to data frames that only has the mont hand year columns 
# (assumes day = 1)
add_date_column_from_day_month_year <- function(inDF){
  inDF$day <- 1
  inDF$date <- as.Date(with(inDF, paste(year,month,day,sep="-")), "%Y-%m-%d")
  return(inDF)
}

########

# creates a dataframe with relative values (intended for the monthly variables)
create_relative_dataframe_of_monthly_values_across_years_V2 <- function(inDF){
  # # FOR TESTING
  # inDF <- flux_data_wombat_byyear
  # create an outout dataframe
  outDF <- inDF

  # find the maximum value and calculate the relative value according to that
  for (col_num in 3:length(colnames(inDF))) {
    data_col <- inDF[[col_num]]
    # find minimum 
    minincol <- min(data_col, na.rm=T)
    # remove the minimum value from all the data
    data_col <- data_col - minincol
    # find the maximum (after the minimum was removed)
    maxincol <- max(data_col, na.rm=T)
    outDF[[col_num]] <- data_col / maxincol
  }
  return(outDF)
}

################

# reshape a dataframe with a year, month columns as well as columns of 
#     different variables
reshape_yearly_monthly_to_rows <- function(inDF){
  # Reshape the data frame
  reshaped_df <- inDF %>% 
    pivot_longer(cols = c(-month,-year), 
                 names_to = "Variable", 
                 values_to = "Value")
  return(reshaped_df)
}

##########

add_PAR_LAI_stff <- function(inDF){
  # this is the library for the smooting fiunction
  library(TTR)
  
  # Smooth the dataset
  inDF$lai <- SMA(inDF$LAI, n=14)
  # change the name of the original LAI column to lai_obs
  colnames(inDF)[4] <- "lai_obs"
  # remove the NAs (as the smoothing average has NANs at the start of the timeseries)
  inDF <- inDF[!is.na(inDF$lai),]
  # calculate increment of the smoothed lai
  outDF <- calc_lai_increment(inDF)
  
  return(outDF)
}

# tests plotting of a variable in a df
testboxplotvar <- function(inDF, varname, site){
  data <- inDF[inDF$Variable == varname,]
  head(data)
  ggplot(data=data)+
    geom_boxplot(aes(x=as.factor(month), y=Value), outlier.shape = NA)+
    theme_bw()+
    ylim(-0.5,0.5)+
    ggtitle(site)
}

remove_trees_from_cup <- function(inDF, cutoff){
  inDF <- BAI_cup 
  cutoff <- 0.5
  # make a list (vector) of all the tree numbers that need to be removed that 
  #   are below or above the cutoff
  trees_to_remove <- c(unique(inDF[inDF$BAI_cm2_day >= cutoff,]$Tree), 
                       unique(inDF[inDF$BAI_cm2_day <= -cutoff,]$Tree))
  # remove the trees that are on the list
  for (i in trees_to_remove){
    inDF <- inDF %>% filter(!(Tree == i))
  }
  return(inDF)
}
