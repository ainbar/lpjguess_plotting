
library(reshape2)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(doBy)
library(patchwork) # to plot several plots as panels
library(TTR) # for smoothing

rm(list = ls())

calc_monthly_inc_value <- function(value, monthval, yearval=1999) {
  monthlyinc <- value * daysinthismonth(monthval, yearval)
  return(monthlyinc)
}  
common_folder <- '/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/'
setwd(paste0(common_folder,'LPJ_allocation_phenology/'))

source("~/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/common_functions.R")

all <- read.csv("all_absolute_byYear_withdate.csv", header=T)
head(all)

sites <- unique(all$site)
sites
variables <- unique(all$Variable)
variables

# set a date column
all$date <- as.Date(paste0(all$year,"-", all$month, "-", "01"))

# find minimum and maximum dates for each site (to adjust LAI and flux data values accordingly)
CUP <- all[all$site == "CUP", ]
min_date_CUP <- min(CUP[CUP$Variable == "BAI_cm2_day",]$date)
max_date_CUP <- max(CUP[CUP$Variable == "BAI_cm2_day",]$date)
Wombat <- all[all$site == "Wombat", ]
min_date_Wombat <- min(Wombat[Wombat$Variable == "BAI_cm2_day",]$date)
max_date_Wombat <- max(Wombat[Wombat$Variable == "BAI_cm2_day",]$date)
Robson <- all[all$site == "Robson", ]
min_date_Robson <- min(Robson[Robson$Variable == "BAI_cm2_day",]$date)
max_date_Robson <- max(Robson[Robson$Variable == "BAI_cm2_day",]$date)

# clip the dates accordingly
CUP <- CUP[CUP$date >= "2011-01-01",]
CUP <- CUP[CUP$date < "2021-01-01",]
Wombat <- Wombat[Wombat$date >= "2012-01-01",]
Wombat <- Wombat[Wombat$date < "2019-01-01",]
Robson <- Robson[Robson$date >= "2016-09-01",]
Robson <- Robson[Robson$date < "2021-01-01",]

all_withindates <- rbind(CUP, Wombat, Robson)

unique(all_withindates$Variable)

# remove the unnecessary variables
subset_vars_allsites <- rbind(all_withindates[all_withindates$Variable == "BAI_cm2_day",], 
                              all_withindates[all_withindates$Variable == "lai_inc",],
                              all_withindates[all_withindates$Variable == "GPP",],
                              all_withindates[all_withindates$Variable == "NEP",],
                              all_withindates[all_withindates$Variable == "ET",], 
                              all_withindates[all_withindates$Variable == "Ta",])
unique(subset_vars_allsites$Variable)
head(subset_vars_allsites)
subset_vars_CP_WBT <- subset_vars_allsites[!subset_vars_allsites$site == "Robson",]
head(subset_vars_CP_WBT)
unique(subset_vars_CP_WBT$site)
# Saummarise by month


all_bymonth <- aggregate(subset_vars_allsites[,"Value"], 
                        by=with(subset_vars_allsites, 
                                list(month=month, site=site, variable=Variable)), 
                        FUN=median, na.rm=T)
all_bymonth_CP_WBT <- aggregate(subset_vars_CP_WBT[,"Value"], 
                         by=with(subset_vars_CP_WBT, 
                                 list(month=month, site=site, variable=Variable)), 
                         FUN=median, na.rm=T)
unique(all_bymonth$site)

names(all_bymonth)[4] <- "value"
names(all_bymonth_CP_WBT)[4] <- "value"


BAI_lai <- rbind(all_bymonth[all_bymonth$variable == "BAI_cm2_day",],
                 all_bymonth[all_bymonth$variable == "lai_inc",])
BAI_lai_temp <- BAI_lai
names(BAI_lai_temp)

for (i in 1:length(BAI_lai_temp$value)) {
  BAI_lai_temp$value[i] <- 
    calc_monthly_inc_value(BAI_lai_temp$value[i], BAI_lai_temp$month[i], 1999)
}

BAI_lai<-BAI_lai_temp
GPP_NEP <- rbind(all_bymonth[all_bymonth$variable == "GPP",],
                 all_bymonth[all_bymonth$variable == "NEP",])
ET_Ta <- rbind(all_bymonth[all_bymonth$variable == "ET",],
               all_bymonth[all_bymonth$variable == "Ta",])
## FOR BOX PLOTS
BAI_lai_box <- rbind(all_withindates[all_withindates$Variable == "BAI_cm2_day",],
                     all_withindates[all_withindates$Variable == "lai_inc",])
BAI_lai_box_temp <- BAI_lai_box
for (i in 1:length(BAI_lai_box_temp$Value)) {
  BAI_lai_box_temp$Value[i] <- calc_monthly_inc_value(BAI_lai_box_temp$Value[i], BAI_lai_box_temp$month[i], 1999)
}
BAI_lai_box<-BAI_lai_box_temp
GPP_NEP_box <- rbind(all_withindates[all_withindates$Variable == "GPP",],
                     all_withindates[all_withindates$Variable == "NEP",])

ET_Ta_box <- rbind(all_withindates[all_withindates$Variable == "ET",],
                     all_withindates[all_withindates$Variable == "Ta",])
# write.csv(rbind(GPP_NEP_box, BAI_lai_box), "all_byyear_bymonth.csv")
##### PLOT LINES

GPP_NEP[GPP_NEP$site == "CUP",]$site <- "Cumberland Plain"
ET_Ta[ET_Ta$site == "CUP",]$site <- "Cumberland Plain"
BAI_lai[BAI_lai$site == "CUP",]$site <- "Cumberland Plain"

GPP_NEP[GPP_NEP$site == "Robson",]$site <- "Robson Ck"
ET_Ta[ET_Ta$site == "Robson",]$site <- "Robson Ck"
BAI_lai[BAI_lai$site == "Robson",]$site <- "Robson Ck"

# Vector of month abbreviations
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Create the plot
g2 <- ggplot(GPP_NEP, aes(x = month)) +
  geom_line(data = subset(GPP_NEP, variable == "GPP"), 
            aes(y = value, color = "GPP"), linewidth = 1) +
  geom_line(data = subset(GPP_NEP, variable == "NEP"), 
            aes(y = value, color = "NEP"), linewidth = 1) +
  geom_point(data = subset(GPP_NEP, variable == "GPP"), 
             aes(y = value, color = "GPP"), size = 2) +
  geom_point(data = subset(GPP_NEP, variable == "NEP"), 
             aes(y = value, color = "NEP"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("GPP" = "blue", "NEP" = "red"), 
                     name = "Variable") +
  scale_y_continuous(name = expression("GPP, gC/m"^2*"/month"),
                     sec.axis = sec_axis(~ . , name = expression("NEP, gC/m"^2*"/month"))) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("Robson Ck", "Cumberland Plain", "Wombat")), ncol = 1, scales = "free_y")

# Create the plot
maxtemp <- max(ET_Ta[ET_Ta$variable == "Ta",]$value)
maxET <- max(ET_Ta[ET_Ta$variable == "ET",]$value)
scales <- maxET/maxtemp
scales <- 100/20
g1 <- ggplot(ET_Ta, aes(x = month)) +
  geom_line(data = subset(ET_Ta, variable == "ET"), 
            aes(y = value, color = "ET"), linewidth = 1) +
  geom_line(data = subset(ET_Ta, variable == "Ta"), 
            aes(y = value*scales, color = "Ta"), linewidth = 1) +
  geom_point(data = subset(ET_Ta, variable == "ET"), 
             aes(y = value, color = "ET"), size = 2) +
  geom_point(data = subset(ET_Ta, variable == "Ta"), 
             aes(y = value*scales, color = "Ta"), size = 2) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("ET" = "blue4", "Ta" = "red4"), 
                     name = "Variable") +
  scale_y_continuous(name = "ET, mm/month", 
                     sec.axis = sec_axis(~ . / scales,  name = "mean Temp, ˚c")) +
  # sec.axis = sec_axis(~ . / scale_factor, name = "Var2")
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue4"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red4"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue4"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red4")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("Robson Ck", "Cumberland Plain", "Wombat")), ncol = 1)

maxBAI <- max(BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value)
maxLAI_inc <- max(BAI_lai[BAI_lai$variable == "lai_inc",]$value)
scales <- maxET/maxtemp
scales <- maxBAI/maxLAI_inc

g3 <- ggplot(BAI_lai, aes(x = month)) +
  geom_line(data = subset(BAI_lai, variable == "BAI_cm2_day"), 
            aes(y = value, color = "BAI_cm2_day"), linewidth = 1) +
  geom_line(data = subset(BAI_lai, variable == "lai_inc"), 
            aes(y = value, color = "lai_inc"), linewidth = 1) +
  geom_point(data = subset(BAI_lai, variable == "BAI_cm2_day"), 
             aes(y = value, color = "BAI_cm2_day"), size = 2) +
  geom_point(data = subset(BAI_lai, variable == "lai_inc"), 
             aes(y = value, color = "lai_inc"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("BAI_cm2_day" = "green4", "lai_inc" = "black"), 
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
  )+
  facet_wrap(~factor(site, levels=c("Robson Ck", "Cumberland Plain", "Wombat")), ncol = 1, scales = "free_y")

g1+g2+g3

########

# no correlation
plot(ET_Ta[ET_Ta$variable == "Ta",]$value, BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value)

# no correlation
plot(ET_Ta[ET_Ta$variable == "Ta",]$value, BAI_lai[BAI_lai$variable == "lai_inc",]$value)
# no correlation
plot(ET_Ta[ET_Ta$variable == "ET",]$value, BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value)
# no correlation
plot(ET_Ta[ET_Ta$variable == "ET",]$value, BAI_lai[BAI_lai$variable == "lai_inc",]$value)
# no correlation
plot(GPP_NEP[GPP_NEP$variable == "GPP",]$value, BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value)
# no correlation
plot(GPP_NEP[GPP_NEP$variable == "GPP",]$value, BAI_lai[BAI_lai$variable == "lai_inc",]$value)
# no correlation
plot(GPP_NEP[GPP_NEP$variable == "NEP",]$value, BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value)
# no correlation
plot(GPP_NEP[GPP_NEP$variable == "NEP",]$value, BAI_lai[BAI_lai$variable == "lai_inc",]$value)
# no correlation
plot(BAI_lai[BAI_lai$variable == "BAI_cm2_day",]$value,
     BAI_lai[BAI_lai$variable == "lai_inc",]$value)
head(BAI_lai)

########
head(GPP_NEP_box)
ggplot(data=GPP_NEP_box)+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=Variable))+
  facet_wrap(.~site, ncol=1, scales = "free_y")+
  theme_bw()+
  xlab("Month")



  

# 

# Create the plot
maxtemp <-  max(ET_Ta_box[ET_Ta_box$Variable == "Ta",]$Value)
maxET <- max(ET_Ta_box[ET_Ta_box$Variable == "ET",]$Value)
scales <- maxET/maxtemp
scales <- 150/50

gb1 <- ggplot(ET_Ta_box, aes(x = as.factor(month))) +
  geom_boxplot(data = ET_Ta_box, 
               aes(y = Value, fill = Variable), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("ET" = "blue4", "Ta" = "red4"), 
                    name = "Variable") +
  scale_y_continuous(name = "ET, mm/month", 
                     sec.axis = sec_axis(~ .,  name = "mean Temp, ˚c")) +
  theme_minimal() +
  theme(
    legend.position = "top", # Remove legend
    axis.title.y.left = element_text(color = "blue4"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red4"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue4"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red4")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("Robson", "Wombat", "CUP")), ncol = 1)

gb2 <- ggplot(GPP_NEP_box, aes(x = as.factor(month))) +
  geom_boxplot(data = GPP_NEP_box, 
            aes(y = Value, fill = Variable), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("GPP" = "blue", "NEP" = "red"), 
                     name = "Variable") +
  scale_y_continuous(name = "Productivity, gC/m2/month") +
  theme_minimal() +
  theme(
    legend.position = "top", # Remove legend
    # axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    # axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    # axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    # axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("Robson", "Wombat", "CUP")), ncol = 1)


gb3 <- ggplot(BAI_lai_box, aes(x = as.factor(month))) +
  geom_boxplot(data = BAI_lai_box, 
            aes(y = Value, fill = Variable), size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_fill_manual(values = c("BAI_cm2_day" = "green4", "lai_inc" = "black"), 
                     name = "Variable") +
  scale_y_continuous(name = "Montly increment")+ 
                     # sec.axis = sec_axis(~ . , name = "LAI increment, m2/m2/month")) +
  # scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "top", # Remove legend
    # axis.title.y.left = element_text(color = "green4"), # Primary y-axis color
    # axis.title.y.right = element_text(color = "black"), # Secondary y-axis color
    # axis.text.y.left = element_text(color = "green4"),  # Primary y-axis ticks color
    # axis.text.y.right = element_text(color = "black")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("Robson", "Wombat", "CUP")), ncol = 1, scales = "free_y")
gb1+gb2+gb3

#########







ggplot(GPP_NEP_ET, aes(x = month)) +
  geom_line(data = subset(GPP_NEP_ET, variable == "GPP"), 
            aes(y = value, color = "GPP"), size = 1, alpha = 0.3) +
  geom_line(data = subset(GPP_NEP_ET, variable == "NEP"), 
            aes(y = value, color = "NEP"), size = 1, alpha = 0.3) +
  geom_line(data = subset(GPP_NEP_ET, variable == "ET"), 
            aes(y = value, color = "ET"), size = 1, alpha = 0.3) +
  geom_point(data = subset(GPP_NEP_ET, variable == "GPP"), 
             aes(y = value, color = "GPP"), size = 2) +
  geom_point(data = subset(GPP_NEP_ET, variable == "NEP"), 
             aes(y = value, color = "NEP"), size = 2) +
  geom_point(data = subset(GPP_NEP_ET, variable == "ET"), 
             aes(y = value, color = "ET"), size = 2) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values = c("GPP" = "black", "NEP" = "black", "ET" = "blue"), 
                     name = "Variable") +
  scale_y_continuous(name = "Productivity, gC/m2/month", 
                     sec.axis = sec_axis(~ . , name = "ET, mm/month")) +
  scale_x_continuous(name = "", breaks = 1:12, labels = month_labels) +
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "black"), # Primary y-axis color
    axis.title.y.right = element_text(color = "blue"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "black"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "blue")   # Secondary y-axis ticks color
  )+
  facet_wrap(~factor(site, levels=c("CUP", "Wombat", "Robson")), ncol = 1)



############# EXAMPLE


# Load the ggplot2 package
library(ggplot2)

# Example dataframe with an additional "year" column
set.seed(123) # For reproducibility
data <- data.frame(
  month = rep(1:12, each = 18),
  year = rep(rep(2020:2022, each = 6), times = 12),
  site = rep(c("Site1", "Site2", "Site3"), each = 6, times = 12),
  variable = rep(c("var1", "var2"), each = 3, times = 36),
  value = c(rnorm(216, mean = 50, sd = 10), rnorm(216, mean = 5, sd = 1))
)

# Vector of month abbreviations
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Create the plot
ggplot(data, aes(x = factor(month, levels = 1:12, labels = month_labels), y = value, fill = variable)) +
  # Boxplots grouped by month and colored by variable
  geom_boxplot(outlier.shape = NA, alpha = 0.7) +
  scale_fill_manual(values = c("var1" = "blue", "var2" = "red"), name = "Variable") +
  scale_y_continuous(name = "Values for var1",
                     sec.axis = sec_axis(~ . / 10, name = "Values for var2")) +
  facet_wrap(~ site, ncol = 1) + # Create a grid of plots, one per site
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  ) +
  labs(
    title = "Monthly Boxplots of Values by Site",
    x = "Month"
  )

######## EXAMPLE WITH CONFIDENCE INTERVALS

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Example dataframe with an additional "year" column
set.seed(123) # For reproducibility
data <- data.frame(
  month = rep(1:12, each = 18),
  year = rep(rep(2020:2022, each = 6), times = 12),
  site = rep(c("Site1", "Site2", "Site3"), each = 6, times = 12),
  variable = rep(c("var1", "var2"), each = 3, times = 36),
  value = c(rnorm(216, mean = 50, sd = 10), rnorm(216, mean = 5, sd = 1))
)

# Vector of month abbreviations
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Calculate mean and confidence intervals
summary_data <- data %>%
  group_by(site, month, variable) %>%
  summarize(
    mean_value = mean(value),
    ci_lower = mean(value) - qt(0.975, df = n() - 1) * sd(value) / sqrt(n()),
    ci_upper = mean(value) + qt(0.975, df = n() - 1) * sd(value) / sqrt(n()),
    .groups = "drop"
  )

# Create the plot
ggplot(summary_data, aes(x = factor(month, levels = 1:12, labels = month_labels), y = mean_value, color = variable, fill = variable, group = variable)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), alpha = 0.2, color = NA) +
  # Add lines
  geom_line(size = 1) +
  scale_color_manual(values = c("var1" = "blue", "var2" = "red"), name = "Variable") +
  scale_fill_manual(values = c("var1" = "blue", "var2" = "red"), name = "Variable") +
  scale_y_continuous(name = "Values for var1",
                     sec.axis = sec_axis(~ . / 10, name = "Values for var2")) +
  facet_wrap(~ site, ncol = 1) + # Create a grid of plots, one per site
  theme_minimal() +
  theme(
    legend.position = "none", # Remove legend
    axis.title.y.left = element_text(color = "blue"), # Primary y-axis color
    axis.title.y.right = element_text(color = "red"), # Secondary y-axis color
    axis.text.y.left = element_text(color = "blue"),  # Primary y-axis ticks color
    axis.text.y.right = element_text(color = "red")   # Secondary y-axis ticks color
  ) +
  labs(
    title = "Monthly Trends with Confidence Intervals by Site",
    x = "Month"
  )

##### SPIDER

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example dataframe with an additional "year" column
set.seed(123) # For reproducibility
data <- data.frame(
  month = rep(1:12, each = 18),
  year = rep(rep(2020:2022, each = 6), times = 12),
  site = rep(c("Site1", "Site2", "Site3"), each = 6, times = 12),
  variable = rep(c("var1", "var2"), each = 3, times = 36),
  value = c(rnorm(216, mean = 50, sd = 10), rnorm(216, mean = 5, sd = 1))
)

# Summarize the data by site, month, and variable
summary_data <- data %>%
  group_by(site, month, variable) %>%
  summarize(mean_value = mean(value), .groups = "drop") %>%
  # Complete to include all months for all sites and variables
  complete(
    site,
    month = 1:12,
    variable,
    fill = list(mean_value = 0) # Fill missing values with 0 or NA if preferred
  )

# Vector of month abbreviations
month_labels <- c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")

# Create the spider plot
ggplot(summary_data, aes(
  x = factor(month, levels = 1:12, labels = 0:11), # Ensure all months are included
  y = mean_value, group = variable, color = variable
)) +
  geom_line(size = 1) + # Draw lines for each variable
  scale_color_manual(values = c("var1" = "blue", "var2" = "red"), name = "Variable") +
  scale_y_continuous(name = NULL, limits = c(0, NA)) + # Optional: Adjust radial scale
  coord_polar(start = 0) + # Convert to polar coordinates
  facet_wrap(~ site) + # One spider plot per site
  theme_minimal() +
  theme(
    legend.position = "top", # Place the legend at the top
    axis.title = element_blank(), # Remove axis titles
    panel.grid.major = element_line(color = "gray80"), # Customize grid lines
    strip.text = element_text(size = 12, face = "bold"), # Style facet labels
    axis.text.x = element_text(size = 10) # Ensure month labels are visible
  ) +
  labs(
    title = "Spider Plots by Site",
    x = NULL, y = NULL # Remove default axis labels
  )


########


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Example dataframe with an additional "year" column
set.seed(123) # For reproducibility
data <- data.frame(
  month = rep(1:12, each = 18),
  year = rep(rep(2020:2022, each = 6), times = 12),
  site = rep(c("Site1", "Site2", "Site3"), each = 6, times = 12),
  variable = rep(c("var1", "var2"), each = 3, times = 36),
  value = c(rnorm(216, mean = 50, sd = 10), rnorm(216, mean = 5, sd = 1))
)

# Summarize the data by site, month, and variable
summary_data <- data %>%
  group_by(site, month, variable) %>%
  summarize(mean_value = mean(value), .groups = "drop") %>%
  # Complete to include all months for all sites and variables
  complete(
    site,
    month = 1:12,
    variable,
    fill = list(mean_value = 0) # Fill missing values with 0 or NA if preferred
  )

# Add a duplicate of January as the 13th month for continuity (optional for barplot)
summary_data <- summary_data %>%
  group_by(site, variable) %>%
  mutate(
    month_label = factor(month, levels = 1:12, labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"))
  )

# Create the circular barplot
ggplot(summary_data, aes(
  x = factor(month, levels = 1:12, labels = month_labels), # Set months as factors
  y = mean_value, fill = variable
)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.8) + # Dodge bars for variables
  scale_fill_manual(values = c("var1" = "blue", "var2" = "red"), name = "Variable") +
  coord_polar(start = 0) + # Convert to polar coordinates
  facet_wrap(~ site) + # Create one plot per site
  theme_minimal() +
  theme(
    legend.position = "top", # Legend on top
    axis.title = element_blank(), # Remove axis titles
    panel.grid.major = element_line(color = "gray80"), # Customize grid lines
    strip.text = element_text(size = 12, face = "bold"), # Style facet labels
    axis.text.x = element_text(size = 10) # Ensure month labels are visible
  ) +
  labs(
    title = "Circular Barplots by Site",
    x = NULL, y = NULL # Remove default axis labels
  )



