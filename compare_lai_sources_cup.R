

lai_par_plot <- read.csv(paste0(folder_common, 'LPJ_allocation_phenology/data/CUP_LAI_20220915.csv'), header = T)
lai_par_plot$Date <- as.Date(lai_par_plot$Date)
lai_par_plot$month <- month(lai_par_plot$Date)
lai_par_plot$year <- year(lai_par_plot$Date)

# in lai_par_cup_byyear, lai is smoothed, and the increment is calcualted
# on the smoothed values

# save backup
lai_par_cup_byyear_temp <- lai_par_cup_byyear
lai_cup_byyear_temp <- lai_cup_byyear
lai_modis_cup_byyear_temp <- lai_modis_cup_byyear
# prepare a plottable datraframe (for boxplot)
lai_par_cup_byyear_temp <- lai_par_cup_byyear_temp[,-c(3:4,7)]
lai_par_cup_byyear_temp$source <- "par"
lai_cup_byyear_temp$source <- "bom"
lai_modis_cup_byyear_temp$source <- "modis"

head(lai_par_cup_byyear_temp)
head(lai_cup_byyear_temp)
head(lai_modis_cup_byyear_temp)

lai_cup_combined <- rbind(lai_par_cup_byyear_temp, 
                          lai_cup_byyear_temp, 
                          lai_modis_cup_byyear_temp)
head(lai_cup_combined)

Variable1 <-lai_cup_combined[,c(1:3,5)]
Variable2 <-lai_cup_combined[,c(1:2,4:5)]
Variable1$Variable <- colnames(Variable1)[3]
Variable2$Variable <- colnames(Variable2)[3]
colnames(Variable1)[3] <- "Value"
colnames(Variable2)[3] <- "Value"

head(Variable1)
head(Variable2)

lai_cup_combined <- rbind(Variable1, Variable2)
head(lai_cup_combined)

# change the order of the dataframes
lai_cup_combined$source <-
  factor(lai_cup_combined$source,
         levels = c("bom", "modis", "par"))

unique(lai_cup_combined$source)


ggplot(data=lai_cup_combined)+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=source))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
  xlab("Month")

ggplot()+
  geom_line(data=lai_cup_modis500[lai_cup_modis500$Date > min(lai_par_plot$Date) &
                                    lai_cup_modis500$Date < max(lai_par_plot$Date),],
            aes(x=Date, y=lai), color= "green3")+
  geom_line(data=lai_cup[lai_cup$Date > min(lai_par_plot$Date) &
                                    lai_cup$Date < max(lai_par_plot$Date),],
            aes(x=Date, y=lai), color="red")+
  geom_point(data=lai_par_plot, aes(x=Date, y=LAI), color="blue")+
  theme_bw()+
  ylab("LAI estimate")

library(ggthemes)
ggplot(data=lai_cup_combined[(lai_cup_combined$source == "par"|
                             lai_cup_combined$source == "modis"),])+
  geom_boxplot(aes(x=as.factor(month), y=Value, fill=scale_color_tableau()))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()+
  xlab("Month")
  
