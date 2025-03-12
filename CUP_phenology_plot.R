
####################
# # # # PLOT # # # #
####################
#
ggplot(data = lai_cup, aes(x=Date, y=lai, color=Season))+
  geom_point()+geom_line(color = "gray")+
  geom_rug()+
  theme_bw()

ggplot(data = lai_cup, aes(x=Date, y=lai_inc, color=Season))+
  geom_point()+geom_line(color = "gray")+
  geom_rug()+
  theme_bw()

# lai anomaly timseries
plot_ly() %>% add_lines(x = lai_cup$Date, y = lai_cup$lai_anomaly)

# LAI anomaly per month
plot_ly() %>% add_boxplot(x = lai_cup$month, y = lai_cup$lai_anomaly)

# LAI anomaly per season
plot_ly() %>% add_boxplot(x = lai_cup$Season, y = lai_cup$lai_anomaly)


########
# plot drivers (Temp, insolation, rainfall?). fluxes, LAI and EucFACE diameter growht together
p011 <- ggplot(data=flux_data_cup, aes(x=date, y=Fsd))+
  geom_point(size=0.1, alpha = 0.5)+geom_line(color = "gray", alpha=.5)+
  theme_bw()+labs(y="Solar Radiation?")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2)

p01 <- ggplot(data=flux_data_cup, aes(x=date, y=Ta))+
  geom_point(size=0.1, alpha = 0.5)+geom_line(color = "gray", alpha=.5)+
  theme_bw()+labs(y="Tair,˚c")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2)

p02 <- ggplot(data=flux_data_cup, aes(x=date, y=GPP))+
  geom_point(size=0.1, alpha = 0.5)+geom_line(color = "gray", alpha=0.5)+
  theme_bw()+labs(y="GPP")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2) 

p03 <- ggplot(data=flux_data_cup, aes(x=date, y=NEP))+
  geom_point(size=0.1, alpha = 0.5)+geom_line(color = "gray", alpha=0.5)+
  theme_bw()+labs(y="NEE")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2) 

p1 <- ggplot(data=output_df_sumdate_0pos, aes(x=Date, y=DBH_inc.medianpertree, color=Season))+
  geom_point()+geom_line(color = "gray")+
  theme_bw()+labs(y="median dDBH/dt, [cm/day]")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

p2 <- ggplot(data = lai_cup, aes(x=Date, y=lai_anomaly, color=Season))+
  geom_point()+geom_line(color = "gray")+labs(y="LAI anomaly")+
  geom_rug()+
  theme_bw()+
  geom_smooth(method = 'gam', formula = y ~ s(x, k = 200, bs = "cs"),
              fill = "gray", color = "black", linetype = 2) +
  theme(panel.border = element_blank(), 
        legend.position = c(.75, .90),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(), axis.line=element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  guides(color = guide_legend(direction = "horizontal"))

## Extract scales
scales <- lapply(list(p011, p01, p02, p03, p1, p2), layer_scales)
## Find range from layer_scales object. It is contained in obj$x$range$range
xlim <- map(scales,~ as.numeric(.x$x$range$range)) %>%
  unlist() %>%
  range()

p011 / p01 / p02 / p03/ p1 / p2 & coord_cartesian(xlim = as.Date(xlim, origin = '1970-01-01'))


####
# creates a dataframe for shading the seasons
source("~/Library/CloudStorage/Dropbox/R_CODE/HIE/EucFACE/DE_dendro/create_season_ranges.R")

alpha_season <- .2

SM <- ggplot()+
  geom_point(data=EF_soil_moisture_raw_summarybyDate, aes(x=Date, y=swc), 
             size=0.1, alpha = 0.9)+
  geom_line(data=EF_soil_moisture_raw_summarybyDate, aes(x=Date, y=swc),
            color = "gray", alpha=.9)+
  theme_bw()+labs(y="SWC")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)

INSOL <- ggplot()+
  geom_line(data=flux_data_cup, aes(x=date, y=Fsd), color = "gray", alpha=.9)+
  theme_bw()+labs(y="Solar Radiation?")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(data=flux_data_cup, aes(x=date, y=Fsd), 
              method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2)+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)

AIR_TEMP <- ggplot()+
  geom_line(data=flux_data_cup, aes(x=date, y=Ta),
            color = "gray", alpha=.9)+
  theme_bw()+labs(y="Tair,˚c")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_smooth(data=flux_data_cup, aes(x=date, y=Ta),
              method = 'gam', formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "black", linetype = 2)+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)

GPP_NEP <- ggplot()+
  geom_line(data=flux_data_cup, aes(x=date, y=GPP),color = "red", alpha=0.9)+
  geom_line(data=flux_data_cup, aes(x=date, y=NEP), color = "blue", alpha=0.9)+
  geom_smooth(data=flux_data_cup, aes(x=date, y=GPP),method = 'gam', 
              formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "red", linetype = 2)+
  geom_smooth(data=flux_data_cup, aes(x=date, y=NEP), method = 'gam', 
              formula = y ~ s(x, k = 100, bs = "cs"),
              fill = "gray", color = "blue", linetype = 2)+
  theme_bw()+ labs(y="GPP, NEE, kg/C/m^2/day")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)

DBH_INC <- ggplot()+
  geom_point(data=output_df_sumdate, aes(x=Date, y=DBH_inc.medianpertree))+
  geom_line(data=output_df_sumdate, aes(x=Date, y=DBH_inc.medianpertree), color = "gray")+
  theme_bw()+
  labs(y="median dDBH/dt, [cm/day]")+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)


LAI_ANOM <- ggplot()+
  geom_point(data = lai_cup, aes(x=Date, y=lai_anomaly))+
  geom_line(data = lai_cup, aes(x=Date, y=lai_anomaly),color = "gray")+
  labs(y="LAI anomaly")+
  theme_bw()+
  theme(panel.border = element_blank(), 
        legend.position = c(.75, .90),
        legend.background = element_rect(fill = "transparent"),
        legend.title = element_blank(), axis.line=element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black")) +
  guides(color = guide_legend(direction = "horizontal"))+
  geom_rect(data = shade_season, aes(xmin = x1, xmax = x2, ymin = min, ymax = max), 
            fill=shade_season$shade, alpha = alpha_season)

## Extract scales
scales <- lapply(list(SM, INSOL, AIR_TEMP, GPP_NEP, DBH_INC, LAI_ANOM), layer_scales)
## Find range from layer_scales object. It is contained in obj$x$range$range
xlim <- map(scales,~ as.numeric(.x$x$range$range)) %>%
  unlist() %>%
  range()


SM/INSOL/AIR_TEMP/GPP_NEP/DBH_INC/LAI_ANOM


#########################
### BOXPLOTS - months ###
#########################

flux_data_cup$month <- as.factor(flux_data_cup$month)
flux_data_cup$Season <-
  factor(flux_data_cup$Season,
         levels = c("WINTER", "SPRING", "SUMMER", "AUTUMN"))
lai_cup$Season <-
  factor(lai_cup$Season,
         levels = c("WINTER", "SPRING", "SUMMER", "AUTUMN"))
output_df_positive <- output_df[!is.na(output_df$BA_inc),]
output_df_positive <- output_df[!is.na(output_df$month),]
output_df_positive <- output_df[output_df$BA_inc >= 0,]
output_df_positive$Season <-
  factor(output_df_positive$Season,
         levels = c("WINTER", "SPRING", "SUMMER", "AUTUMN"))

SM_df <- EF_soil_moisture_raw_summarybyDate
SM_df$month <- as.factor(SM_df$month)
SM_df$Season <- as.factor(SM_df$Season)
SM_df$Season <-
  factor(SM_df$Season,
         levels = c("WINTER", "SPRING", "SUMMER", "AUTUMN"))

BOX_SM <- ggplot()+
  geom_boxplot(data=SM_df, aes(x=month, y=swc, fill=Season))+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

BOX_INSOL <- ggplot()+
  geom_boxplot(data=flux_data_cup, aes(x=month, y=Fsd, fill=Season), outliers = FALSE)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

BOX_AIR_TEMP <- ggplot()+
  geom_boxplot(data=flux_data_cup, aes(x=month, y=Ta, fill=Season), outliers = FALSE)+
  theme_bw()+
  theme(panel.border = element_blank(), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"))

BOX_GPP_NEE <- ggplot()+
  geom_boxplot(data=flux_data_cup, aes(x=month, y=GPP, fill=Season), outliers = FALSE, color = "gray")+
  geom_boxplot(data=flux_data_cup, aes(x=month, y=NEP, fill=Season), outliers = FALSE)+
  theme_bw()+ 
  labs(y="GPP, NEP, kg/C/m^2/day")+ 
  theme(panel.border = element_blank(),axis.line=element_blank(),legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))


BOX_LAI_INC <- ggplot(data=lai_cup, aes(as.factor(month), lai_inc, fill=Season))+
  geom_boxplot(outliers = FALSE)+
  labs(y="lai incrmnt")+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line=element_blank(),legend.position = "none",
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))

BOX_BA_INC <- ggplot(data=output_df_positive, aes(as.factor(month), BA_inc, fill=Season))+
  geom_boxplot(outliers = FALSE)+
  labs(y="mean dBA/dt")+
  theme_bw()+
  theme(panel.border = element_blank(),axis.line=element_blank(),
        axis.line.y = element_line(size = 0.5, linetype = "solid", colour = "black"),
        axis.line.x = element_line(size = 0.5, linetype = "solid", colour = "black"))

BOX_SM+BOX_INSOL+BOX_AIR_TEMP+BOX_GPP_NEE+BOX_LAI_INC+BOX_BA_INC

################################
# plot relative monthly values #
################################
unique(cup_relative_montly_df_rows$Variable)

cup_relative_montly_df_rows$Variable <-
  factor(cup_relative_montly_df_rows$Variable,
         levels = c("Ta", "Fsd", "Sws", "precip", "ET", "swc", 
                    "GPP", "NEP", "ER", "lai", "lai_inc", "BA_inc","DBH_inc"))
cup_relative_montly_df_rows$Variable <-
  factor(cup_relative_montly_df_rows$Variable,
         levels = c("Ta", "Fsd", "Sws", "precip", "ET", "swc", 
                    "GPP", "NEP", "ER", "lai", "lai_inc", "BA_inc","DBH_inc"))

ggplot(data=cup_relative_montly_df_rows)+
  geom_line(aes(x=month, y=Value, col=Variable))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()

ggplot(data=cup_relative_montly_df_rows)+
  geom_boxplot(aes(x=as.factor(month), y=Value))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()

ggplot(data=cup_relative_montly_df_rows)+
  geom_boxplot(aes(x=as.factor(month), y=Value))+
  geom_line(data=cup_relative_montly_df_rows, aes(x=month, y=Value, col=Variable))+
  geom_point(data=cup_relative_montly_df_rows, aes(x=month, y=Value, col=Variable))+
  facet_wrap(.~Variable, scales = "free_y")+
  theme_bw()






