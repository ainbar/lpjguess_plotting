# plotly for monthly outputs
library(plotly)
library(ncdf4)

rm(list = ls())

# reshapes the dataframe to be in rows (for plotting in ggplot).
reshape_cols_to_rows <- function (inDF) {
  # Reshape the data frame
  reshaped_df <- inDF %>%
    pivot_longer(cols = -Year, names_to = "Month", values_to = "Value")
  return(reshaped_df)
}

# convert month name to month number
monthname2monthnumber <- function(inDF) {
  # inDF<-newdf
  inDF[inDF$Month == "Jan",]$Month <- "01"
  inDF[inDF$Month == "Feb",]$Month <- "02"
  inDF[inDF$Month == "Mar",]$Month <- "03"
  inDF[inDF$Month == "Apr",]$Month <- "04"
  inDF[inDF$Month == "May",]$Month <- "05"
  inDF[inDF$Month == "Jun",]$Month <- "06"
  inDF[inDF$Month == "Jul",]$Month <- "07"
  inDF[inDF$Month == "Aug",]$Month <- "08"
  inDF[inDF$Month == "Sep",]$Month <- "09"
  inDF[inDF$Month == "Oct",]$Month <- "10"
  inDF[inDF$Month == "Nov",]$Month <- "11"
  inDF[inDF$Month == "Dec",]$Month <- "12"
  return(inDF)
}

# create the x axis for the realative PAI valures at Wombat
create_date_from_month_and_year <- function(year_vec, month_vec){
  new_timevec <- as.Date(paste0(year_vec,'-',month_vec,'-01'))
  return(new_timevec)
}

## ------------------
## Simulations
## ------------------
## one PFT
#basepath <- "/Users/30044953/Documents/LPJ_GUESS/Code/lpjguess_dave_daily_grass_photosynthesis/benchmarks/ozflux"
basepath <- "/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux"
#site     <- "Otway"
site     <- "CapeTribulation"
#site     <- "SturtPlains"
# site     <- "ClimGrass"
# site     <- "Tumbarumba"
#site     <- "DalyPasture"
#site     <- "Gingin"
#site     <- "YarramundiDegraded"
#site     <- "DryRiver" 
# site     <- "WombatStateForest"
#site     <- "Samford"
#site     <- "Tumbarumba"
# site     <- "Calperum"
site     <- "CumberlandPlain"
# site <- "WallabyCreek"
# site <- "RobsonCreek"
# site <- "AliceSpringsMulga"
# site <- "SilverPlains"
setwd(paste0(basepath,'/',site,'/out'))

vars_monthly  <- c("maet.out", "mgpp.out","mlai.out", "mnee.out", "mnpp.out")

variable_to_plot <- vars_monthly

version <- "out"   ## default is 'out'

out <- list()
for (var in variable_to_plot){
  tempdf <- read.table(paste0(basepath,"/",site,"/",version,"/",var), header=T)
  newdf <- data.frame(matrix(ncol=3, nrow=0))
  colnames(newdf) <- c("year", "month", "value")
  newdf <- reshape_cols_to_rows(tempdf[,3:15])
  newdf <- monthname2monthnumber(newdf)
  out[[var]] <- newdf
  if (var == variable_to_plot[1]) {
    # save the first file name
    vars_to_plot <- var
  }
  # add the file name to the vector of file names to be plotted
  vars_to_plot <- c(vars_to_plot, var)

}

########## ########## ########## 
########## BOXPLOT ########## 
########## ########## ########## 
# plot one by one using plotly
fig_boxplot <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_boxplot <- fig_boxplot  %>% 
    add_boxplot(x = out[[var]]$Month, y = out[[var]]$Value, name = var)
  # add_lines(x = season, y = out[[var]][,pft], name = var)
}
fig_boxplot


########## ########## ########## 
########## TIMESERIES ########## 
########## ########## ########## 
timeseries <-create_date_from_month_and_year (
  out[[var]]$Year,
  out[[var]]$Month)

# plot one by one using plotly
fig_timeseries <- plot_ly() 
# %>% layout(yaxis = list(range=c(0,1.2)))
for (var in vars_to_plot){
  print(var)
  # plot the variable
  fig_timeseries <- fig_timeseries  %>% 
    add_lines(x = timeseries, y = out[[var]]$Value, name = var)
  # add_lines(x = season, y = out[[var]][,pft], name = var)
}
fig_timeseries
