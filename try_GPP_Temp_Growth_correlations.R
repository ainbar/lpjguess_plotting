rm(a)
a <- cbind(subset(ET_Ta, variable == "Ta"), subset(BAI_lai, variable == "BAI_cm2_day")$value, subset(GPP_NEP, variable == "GPP")$value)
head(a)
names(a)[4] <- "Ta"
names(a)[5] <- "BAI_cm2_day"
names(a)[6] <- "GPP"
a <- a[-3]
g1 <- ggplot(data=a)+
  geom_point(aes(x=Ta, y=BAI_cm2_day, color=site))+
  facet_wrap(.~site, ncol=1, scales="free_y")

g2 <-ggplot(data=a)+
  geom_point(aes(x=Ta, y=GPP, color=site))+
  facet_wrap(.~site, ncol=1, scales="free_y")
g1+g2


#########


bainc <- all_withindates[all_withindates$Variable == "BAI_cm2_day",]
bainc$Value <- bainc$Value * (365/12)
Ta_GPP <- all_withindates[all_withindates$Variable == "Ta",]
head(Ta_GPP)
Ta_GPP <- Ta_GPP[-c(1:4)]
names(Ta_GPP)[1] <- "Temp"
Ta_GPP <- cbind(Ta_GPP, all_withindates[all_withindates$Variable == "GPP",]$Value)
names(Ta_GPP)[4] <- "GPP"

head(Ta_GPP)

median_diameter <- bainc %>%
  group_by(site, date) %>%
  summarize(Median_Diameter = median(Value, na.rm = TRUE), .groups = 'drop')

combined_data <- Ta_GPP %>%
  left_join(median_diameter, by = c("site", "date"))

head(combined_data)
combined_data <- combined_data %>%
  fill(Median_Diameter, .direction = "downup")  # Fill both directions

library(ggplot2)
library(dplyr)

# Filter data by site and plot
ggplot(combined_data, aes(x = Temp)) +
  # GPP on the left y-axis
  geom_point(aes(y = GPP, color = "GPP"), alpha = 0.7) +
  # Median Diameter on the right y-axis
  geom_point(aes(y = Median_Diameter / max(Median_Diameter, na.rm = TRUE) * max(GPP, na.rm = TRUE), 
                 color = "Median Diameter"), alpha = 0.7) +
  # Facet by site
  facet_wrap(~ site, ncol=1, scales = "free_y") +
  scale_y_continuous(
    name = "GPP",
    sec.axis = sec_axis(~ . * max(combined_data$Median_Diameter, na.rm = TRUE) / max(combined_data$GPP, na.rm = TRUE), name = "Median Diameter")
  ) +
  labs(
    x = "Temperature",
    color = "Legend"
  ) +
  theme_minimal() +
  theme(legend.position = "top")




ggplot(data=combined_data, aes(x=Temp))+
  geom_point(aes(y=Median_Diameter, color=site))+
  geom_point(aes(y=GPP, color=site), shape=18)+
  scale_y_continuous(name = expression("GPP, gC/m"^2*"/month"),
                     sec.axis = sec_axis(~., name = expression("d")))+
  facet_wrap(.~site, ncol=1, scales="free_y")

