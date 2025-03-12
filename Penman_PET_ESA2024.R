

fold <- "/Users/30060406/Library/CloudStorage/Dropbox/R_CODE/HIE/LPJ_allocation_phenology/data/Penman_calcs/"
pfile_rc <- "RobsonCreek_Penman.csv"
pfile_cup <- "CumberlandPlain_Penman.csv"
pfile_womb <-"WombatStateForest_Penman.csv"
pet_rc <- read.csv(paste0(fold, pfile_rc), header=T)
pet_cup <- read.csv(paste0(fold, pfile_cup), header=T)
pet_womb <- read.csv(paste0(fold, pfile_womb), header=T)


prep_Penman <- function(inDF) {
  inDF$date <- as.Date(inDF$date)
  inDF$month <- month(inDF$date)
  inDF$year <- year(inDF$date)
  return(inDF)
}

pet_rc <- prep_Penman(pet_rc)
pet_rc$site <- "Robson Ck"
pet_cup <- prep_Penman(pet_cup)
pet_cup$site <- "Cumberland Plain"
pet_womb <- prep_Penman(pet_womb)
pet_womb$site <- "Wombat Forest"

pet_all <- rbind(pet_rc, pet_cup, pet_womb)
# head(pet_all)

pet_all_year_montly <- aggregate(pet_all[,"PenmanPET"], 
                         by=with(pet_all, 
                                 list(month=month, year=year, site=site)),
                         FUN=sum, na.rm=T)
names(pet_all_year_montly)[4] <- "PenmanPET"
head(pet_all_year_montly)
pet_all_year_montly$date <- as.Date( paste0( pet_all_year_montly$year,"-",pet_all_year_montly$month,"-", "01" ) )


pet_all_montly <- aggregate(pet_all_year_montly[,"PenmanPET"], 
                         by=with(pet_all_year_montly, 
                                 list(month=month, site=site)),
                         FUN=median, na.rm=T)
names(pet_all_montly)[3] <- "PenmanPET"

# ggplot(data=pet_all_montly)+
#   geom_point(aes(x=month, y=PenmanPET, color=site))

## RAINFALL
all <- read.csv("all_absolute_byYear_withdate.csv", header=T)
p <- all[all$Variable=="precip",]
head(p)

p_all_year_montly <- aggregate(p[,"Value"], by=with(p, list(month=month, year=year, site=site)), FUN=sum, na.rm=T)
names(p_all_year_montly)[4] <- "P"

p_all_year_montly[p_all_year_montly$site == "CUP",]$site = "Cumberland Plain"
p_all_year_montly[p_all_year_montly$site == "Robson",]$site = "Robson Ck"
p_all_year_montly[p_all_year_montly$site == "Wombat",]$site = "Wombat Forest"

p_all_year_montly$date <- as.Date( paste0( p_all_year_montly$year,"-",p_all_year_montly$month,"-", "01" ) )
head(p_all_year_montly)
p_montly <- aggregate(p[,"Value"], by=with(p, 
                                  list(month=month, site=site)),
                          FUN=median, na.rm=T)
names(p_montly)[3] <- "P"

min(p_all_year_montly[p_all_year_montly$site == "Cumberland Plain", ]$date)
max(p_all_year_montly[p_all_year_montly$site == "Cumberland Plain", ]$date)
min(pet_all_year_montly[pet_all_year_montly$site == "Cumberland Plain", ]$date)
max(pet_all_year_montly[pet_all_year_montly$site == "Cumberland Plain", ]$date)


min(p_all_year_montly[p_all_year_montly$site == "Robson Ck", ]$date)
max(p_all_year_montly[p_all_year_montly$site == "Robson Ck", ]$date)
min(pet_all_year_montly[pet_all_year_montly$site == "Robson Ck", ]$date)
max(pet_all_year_montly[pet_all_year_montly$site == "Robson Ck", ]$date)


min(p_all_year_montly[p_all_year_montly$site == "Wombat Forest", ]$date)
max(p_all_year_montly[p_all_year_montly$site == "Wombat Forest", ]$date)
min(pet_all_year_montly[pet_all_year_montly$site == "Wombat Forest", ]$date)
max(pet_all_year_montly[pet_all_year_montly$site == "Wombat Forest", ]$date)



### COMBINE MONTHLY
combined_monthly <- cbind(pet_all_montly, p_montly[,3])
names(combined_monthly)[4] <- "P"
combined_monthly$wetness <- combined_monthly$P/combined_monthly$PenmanPET
# head(combined_monthly)

ggplot(data=combined_monthly)+
  geom_point(aes(x=month, y=wetness, color=site), size=4)+
  scale_x_continuous(
    breaks = 1:12
  ) 

