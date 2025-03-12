# NOW WE NEED TO THINK WHAT TO DO WITH THE ZEROS IN THE INCREMENT
# CONCLUSION: REMOVE THE NEGATIVE INCREMENT BECAUSE I AM *NOT* INTERESTED IN NO 
# GROWTH. ZEROS ARE KEPT BECAUSE NO GROWTH SHOULD BE PART OF THE STATISTICS.
# ***THIS IS SOLELY FOR MY PURPOSE HERE***
# # # # # # # # 
output_df_positive <- output_df[output_df$BA_inc >= 0 & output_df$DBH_inc >= 0,] 

ggplot(output_df[output_df$Tree <= 125,], aes(x=Date, y=cumul_BA_inc))+
  geom_line()+
  facet_wrap( .~Tree, nrow=10, ncol=10, scales = "free_y")+
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 8, angle = 45),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 8))

ggplot(output_df[output_df$Tree >200 & output_df$Tree <300 ,], aes(x=Date, y=cumul_BA_inc))+
  geom_line()+
  facet_wrap( .~Tree, nrow=10, ncol=10, scales = "free_y")+
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 8, angle = 45),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 8))

ggplot(output_df[output_df$Tree >300 & output_df$Tree <400 ,], aes(x=Date, y=cumul_DBH_inc))+
  geom_point()+
  facet_wrap( .~Tree, nrow=10, ncol=10, scales = "free_y")+
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 8, angle = 45),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 8)) + 
  geom_smooth(method='lm')
  

## basal area (BA) increment when negative values are removed
fig <- plot_ly() %>% add_boxplot(x=month(output_df_positive$Date), y=output_df_positive$BA_inc)
fig

plot_ly() %>% add_boxplot(x=month(output_df_positive$Date), y=output_df_positive$DBH_inc)

plot_ly() %>% add_boxplot(x=output_df_positive$Season, y=output_df_positive$DBH_inc)

## basal area (BA) increment including negative values
plot_ly() %>% add_boxplot(x=month(output_df$Date), y=output_df$BA_inc)
# plot both on the same plot
output_df0positive <- output_df
output_df0positive_temp <- output_df
output_df0positive$range <- ">=0"
output_df0positive[output_df0positive$BA_inc < 0 &
                     !is.na(output_df0positive$BA_inc), ]$BA_inc <- NA
output_df0positive_temp$range <- "all"
output_df0positive <- rbind(output_df0positive, output_df0positive_temp)

fig <- plot_ly(output_df0positive, x = ~month(output_df0positive$Date), y = ~BA_inc, color = ~range, type = "box")
fig <- fig %>% layout(boxmode = "group")
fig

## CREATE SUMMEY PLOT
# High Qartile
Hq <- .75
# Low Quartile
Lq <- 1 - Hq
output_df_summary_medians <- output_df %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = median(BA_inc, na.rm = TRUE))
colnames(output_df_summary_medians)[1] <- "Month"
output_df_summary_Hq <- output_df %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = quantile(BA_inc, Hq, na.rm = TRUE))
colnames(output_df_summary_Hq)[1] <- "Month"
output_df_summary_Lq <- output_df %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = quantile(BA_inc, Lq, na.rm = TRUE))
colnames(output_df_summary_Lq)[1] <- "Month"


ggplot()+
  geom_point(data = output_df_summary_medians, aes(x=Month, y=mean_BA_inc),color = "firebrick", shape = "diamond", size = 2) +
  geom_line(data = output_df_summary_medians, aes(x=Month, y=mean_BA_inc), color = "firebrick", linetype = "dotted", lwd = .3) +
  geom_line(data = output_df_summary_Hq, aes(x=Month, y=mean_BA_inc), color = "black", linetype = "dotted", lwd = .3)+
  geom_line(data = output_df_summary_Lq, aes(x=Month, y=mean_BA_inc), color = "black", linetype = "dotted", lwd = .3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  ggtitle(paste0("Median BA increment between ", Lq*100,"th and ", Hq*100,"th precentile" ))+
  ylab("Basal Area Increment, cm^2/day")

# now only for positive (and 0) BA increments
# High Qartile
Hq <- .75
# Low Quartile
Lq <- 1 - Hq
output_df_summary_medians <- output_df[output_df$BA_inc >= 0 & !is.na(output_df$BA_inc),] %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = median(BA_inc, na.rm = TRUE))
colnames(output_df_summary_medians)[1] <- "Month"
output_df_summary_Hq <- output_df[output_df$BA_inc >= 0 & !is.na(output_df$BA_inc),] %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = quantile(BA_inc, Hq, na.rm = TRUE))
colnames(output_df_summary_Hq)[1] <- "Month"
output_df_summary_Lq <- output_df[output_df$BA_inc >= 0 & !is.na(output_df$BA_inc),] %>%
  group_by(month(Date)) %>%
  summarize(mean_BA_inc = quantile(BA_inc, Lq, na.rm = TRUE))
colnames(output_df_summary_Lq)[1] <- "Month"


ggplot()+
  geom_point(data = output_df_summary_medians, aes(x=Month, y=mean_BA_inc),color = "firebrick", shape = "diamond", size = 2) +
  geom_line(data = output_df_summary_medians, aes(x=Month, y=mean_BA_inc), color = "firebrick", linetype = "dotted", lwd = .3) +
  geom_line(data = output_df_summary_Hq, aes(x=Month, y=mean_BA_inc), color = "black", linetype = "dotted", lwd = .3)+
  geom_line(data = output_df_summary_Lq, aes(x=Month, y=mean_BA_inc), color = "black", linetype = "dotted", lwd = .3)+
  theme_bw()+
  scale_x_continuous(breaks = seq(1, 12, by = 1))+
  ggtitle(paste0("Median BA increment (w/o stem shrinkage) between ",
                 Lq*100,"th and ", Hq*100,"th precentile" ))+
  ylab("Basal Area Increment, cm^2/day")


head(output_df)
## look at ring and CO treatemnt effects (none)
output_df$Ring<-as.factor(output_df$Ring)
output_df$month<-as.factor(output_df$month)

ggplot(output_df, aes(x=month, y=BA_inc)) +
  geom_boxplot(aes(fill=Ring), outlier.shape = NA)+
  ylim(-.2, .2)

ggplot(output_df, aes(x=month, y=BA_inc)) +
  geom_boxplot(aes(fill=CO2.trt), outlier.shape = NA)+
  ylim(-.1,.1)

# all measurents 
a<-output_df
a$Date<-as.factor(a$Date)
ggplot(a, aes(x=Date, y=BA_inc)) +
  geom_boxplot(outlier.shape = NA)+
  ylim(-.1,.1)+
  theme(axis.text.x = element_text(face = "bold", color = "#993333", 
                                   size = 8, angle = 45),
        axis.text.y = element_text(face = "bold", color = "blue", 
                                   size = 8))

# Timeseries
gg11 <- ggplot(data=output_df_sumdate_CO2trt, aes(x=Date, y=DBH_inc.medianpertree, col=CO2.trt))+
  geom_point()+ geom_line() +
  ylab("DBH increment per tree, cm/day")+theme_bw()
gg12 <- ggplot(data=output_df_sumdate_CO2trt, aes(x=Date, y=BA_inc.medianpertree, col=CO2.trt))+
  geom_point()+ geom_line() +
  ylab("BA increment per tree, cm^2/day")+theme_bw()
gg13 <- ggplot(data=output_df_sumdate_CO2trt, aes(x=Date, y=DBH_inc.tot, col=CO2.trt))+
  geom_point()+ geom_line() +
  ylab("total DBH increment, cm/day")+theme_bw()
gg14 <- ggplot(data=output_df_sumdate_CO2trt, aes(x=Date, y=BA_inc.tot, col=CO2.trt))+
  geom_point()+ geom_line() +
  ylab("total BA increment, cm^2/day")+theme_bw()

library(patchwork)

(gg11 + gg12) / (gg13 + gg14)

# na.omit(BA_inc.tot)

output_df_sumdate$Season <-
  factor(output_df_sumdate$Season,
         levels = c("WINTER", "SPRING", "SUMMER", "AUTUMN"))

gg11 <- ggplot(data=output_df_sumdate, aes(x=Date, y=DBH_inc.medianpertree, color=Season))+
  geom_point()+ geom_line(color="blue",linetype = "dotted") +
  ylab("DBH increment per tree, cm/day")+theme_bw()
gg12 <- ggplot(data=output_df_sumdate, aes(x=Date, y=BA_inc.medianpertree, color=Season))+
  geom_point()+ geom_line(color="blue",linetype = "dotted") +
    ylab("BA increment per tree, cm^2/day")+theme_bw()
gg13 <- ggplot(data=output_df_sumdate, aes(x=Date, y=DBH_inc.tot, color=Season))+
  geom_point()+ geom_line(color="blue",linetype = "dotted") +
  ylab("total DBH increment, cm/day")+theme_bw()
gg14 <- ggplot(data=output_df_sumdate, aes(x=Date, y=BA_inc.tot, color=Season))+
  geom_point()+ geom_line(color="blue",linetype = "dotted") +
  ylab("total BA increment, cm^2/day")+theme_bw()+geom_rug()

(gg11 + gg12) / (gg13 + gg14)
