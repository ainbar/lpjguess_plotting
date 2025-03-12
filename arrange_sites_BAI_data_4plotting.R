# These are the files that we need to run in the Box_plot.R code

# cup (eucFACE) BA and DBH incremetnt per tree per month per year.
# head(cup_DBH_BA_inc_per_tree_byyear)
BAI_cup <- cup_DBH_BA_inc_per_tree_byyear[,c(1:3,5)]
colnames(BAI_cup)[4] <- "BAI_cm2_day"
BAI_cup$site <- "Cumberland Plains"
head(BAI_cup)
# Wombat BA incremetnt per tree per month per year. (2012-2018) - from Nina plus control site
# !!!!!! note: need to run wombat_dendro.R !!!!
# head(Wombat_BA_inc_per_tree_byyear)
# <- !!!! check this DF: Wombat_NHN_combined_timeseries_byYear_withmonthly
head(Wombat_NHN_combined_timeseries_per_tree_byYear_SDD)
BAI_wombat <- Wombat_NHN_combined_timeseries_per_tree_byYear_SDD
colnames(BAI_wombat)[3] <- "Tree"
colnames(BAI_wombat)[4] <- "BAI_cm2_day"
BAI_wombat$site <- "Wombat Forest"
head(BAI_wombat)
# robson creek BA incremetnt per tree per month per year.
head(rc_pertree_peryear_permonth)
BAI_robson <- rc_pertree_peryear_permonth[,c(2,1,3,4)]
colnames(BAI_robson)[4] <-  "BAI_cm2_day"
BAI_robson$site <- "Robson Ck."

BAI_sites <- rbind(BAI_cup, BAI_wombat, BAI_robson)
head(BAI_sites)

ggplot(data=BAI_sites)+
  geom_boxplot(aes(x=as.factor(month), y=BAI_cm2_day, fill=site), outlier.shape = NA)+
  facet_wrap(.~site)+
  theme_bw()+
  ylim(-0.25,0.5)
