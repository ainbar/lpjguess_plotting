
################################
########    PLOTTING   ########
##############################
# box-plot of data (all trees)
ggplot(data=Wombat_NHN_combined_timeseries_byYear)+
  geom_boxplot(aes(x=factor(month), y=BAI_cm2_day))+
  theme_bw()+
  ggtitle("including negative increments")

# aggregated timeseries (all trees)
ggplot(data=Wombat_NHN_combined_timeseries_byYear)+
  geom_line(aes(x=date, y=BAI_cm2_day))+
  theme_bw()

# aggregated timeseries by species (all trees)
ggplot(data=Wombat_NHN_combined_timeseries_bySpecies)+
  geom_line(aes(x=date, y=BAI_cm2_day, color=Species))+
  geom_line(data=Wombat_NHN_combined_timeseries_byYear, 
            aes(x=date, y=BAI_cm2_day))+ theme_bw()
