# RUN plot_ESA2024.R first


unique(all_bymonth_CP_WBT$variable)
a <- data.frame(Ta = all_bymonth_CP_WBT[all_bymonth_CP_WBT$variable == "Ta",]$value,
                BAI_cm2_day = all_bymonth_CP_WBT[all_bymonth_CP_WBT$variable == "BAI_cm2_day",]$value,
                site = all_bymonth_CP_WBT[all_bymonth_CP_WBT$variable == "BAI_cm2_day",]$site,
                month = all_bymonth_CP_WBT[all_bymonth_CP_WBT$variable == "BAI_cm2_day",]$month)

ggplot(data=a)+
  geom_point(aes(x=Ta, y=BAI_cm2_day, col=as.factor(month), size=1))+ 
  # geom_line(aes(x=Ta, y=BAI_cm2_day))+
  facet_wrap(.~site, scales = "free_y")

library(dplyr)
library(tidyr)
library(lubridate)

Ta <- subset( subset_vars_CP_WBT, Variable == "Ta")
head(Ta)
Ta <- Ta[,c(2,3,4,6,5,7)]
BAI_cm2_day <- subset( subset_vars_CP_WBT, Variable == "BAI_cm2_day")
BAI_cm2_day <- aggregate(BAI_cm2_day[,"Value"], 
          by=with(BAI_cm2_day, list(month=month, year=year, Variable=Variable, site=site)), 
          FUN=median, na.rm=T)
BAI_cm2_day$date <- as.Date(paste0(BAI_cm2_day$year, "-", BAI_cm2_day$month ,"-", "01"))
head(BAI_cm2_day)
names(BAI_cm2_day)[5] <- "Value"

head(Ta)
head(BAI_cm2_day)

Ta_BA <- rbind(Ta, BAI_cm2_day)

Ta_BA_noNA <- Ta_BA[!is.na(Ta_BA$Value),]
head(Ta_BA_noNA)





valid_dates <- Ta_BA_noNA %>%
  group_by(date, site) %>%
  filter(!is.na(Value)) %>%               # Remove rows with NA values in 'value'
  summarise(variable_count = n_distinct(Variable), .groups = "drop") %>%
  filter(variable_count == 2) %>%        # Keep only date-site pairs with both variables
  pull(date)

df_filtered <- Ta_BA_noNA %>%
  filter(date %in% valid_dates & !is.na(Value))

df_wide <- df_filtered %>%
  pivot_wider(
    names_from = Variable, 
    values_from = Value
  ) %>%
  filter(!is.na(BAI_cm2_day) & !is.na(Ta)) %>% # Ensure no NAs remain
  select(date, site, BAI_cm2_day, Ta)

get_season <- function(date) {
  month <- month(date)
  if (month %in% c(12, 1, 2)) {
    return("Summer")
  } else if (month %in% c(3, 4, 5)) {
    return("Autumn")
  } else if (month %in% c(6, 7, 8)) {
    return("Winter")
  } else {
    return("Spring")
  }
}


head(df_wide)


df_wide <- df_wide %>%
  mutate(season = sapply(date, get_season))

df_season <- df_wide

df_season$season <- 
  factor(df_season$season, levels=c('Summer', 'Autumn', 'Winter', 'Spring'))


ggplot(data = df_season)+
  geom_point(aes( x=Ta, y=BAI_cm2_day, 
                  col=as.factor(season), size = as.factor(month(date)) ))+
  facet_wrap(.~site, ncol=1, scales="free_y")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  labs(x="Temperature, ˚c", y="BA inc, cm2/day/tree")+
  theme_bw()

ggplot(data=df_season)+
  geom_boxplot(aes(x=as.factor(month(date)), y=BAI_cm2_day, fill=as.factor(season)))+
  facet_wrap(.~site, ncol=1, scales="free_y")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  labs(x="month", y="BA inc, cm2/day/tree")+
  theme_bw()

ggplot(data=df_season)+
  geom_boxplot(aes(x=as.factor(season), y=BAI_cm2_day, fill=as.factor(season)))+
  facet_wrap(.~site, ncol=1, scales="free_y")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  labs(x="season", y="BA inc, cm2/day/tree")+
  theme_bw()

ggplot(data=df_season, aes(x=date, y=BAI_cm2_day, color=as.factor(season), shape=season))+
  geom_point(size=4)+
  # geom_line(data=df_wide, aes(x=date, y=BAI_cm2_day))+
  facet_wrap(.~site, ncol=1, scales="free_y")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  geom_rug(position = "jitter")+
  labs(x="date", y="BA inc, cm2/day/tree")+
  theme_bw()


ggplot()+
  geom_point(data=df_season, aes(x=date, y=BAI_cm2_day, color=as.factor(season), shape=season), size=4)+
  geom_line(data=df_season, aes(x=date, y=BAI_cm2_day))+
  facet_wrap(.~site, ncol=1, scales="free_y")+
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
  geom_rug(position = "jitter")+
  labs(x="date", y="BA inc, cm2/day/tree")+
  theme_bw()
