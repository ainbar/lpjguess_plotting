###########################
######### WOMBAT #########
########################


###########
### PAI ###
###########
# create the x axis for the realative PAI valures at Wombat
create_date_from_month_and_year <- function(year_vec, month_vec){
  new_timevec <- as.Date(paste0(year_vec,'-',month_vec,'-01'))
  return(new_timevec)
}

time_axis_relPAI_wombat <-create_date_from_month_and_year (
  relative_Wombat_PAI_inc_byyear_reshaped$year,
  relative_Wombat_PAI_inc_byyear_reshaped$month)
  
time_axis_absPAI_wombat <-create_date_from_month_and_year (
  Wombat_PAI_inc_byyear$year,
  Wombat_PAI_inc_byyear$month)

p1 <- ggplot(data=Wombat_PAI_inc_byyear)+
  geom_point(aes(x=time_axis_absPAI_wombat, y=pai_inc))+
  geom_line(aes(x=time_axis_absPAI_wombat, y=pai_inc))+
  theme_bw()+ labs(x="date", y="absolute dPAI/dt")

p2 <- ggplot()+
  geom_point(data=relative_Wombat_PAI_inc_byyear_reshaped, 
             aes(x=time_axis_relPAI_wombat, y=Value))+
  geom_line(data=relative_Wombat_PAI_inc_byyear_reshaped, 
            aes(x=time_axis_relPAI_wombat, y=Value))+
  theme_bw()+ labs(x="date", y="realtive dPAI/dt")





p4 <- ggplot()+
  geom_boxplot(data=relative_Wombat_PAI_inc_byyear_reshaped,
               aes(x=as.factor(month), y=Value))+
  geom_point(data=relative_Wombat_PAI_inc_byyear_reshaped,
             aes(x=as.factor(month), y=Value, colour=year))+
  theme_bw()+ 
  labs(x="month", y="realtive dPAI/dt")


p3 <- ggplot()+
  geom_boxplot(data=Wombat_PAI_inc_byyear,
               aes(x=as.factor(month), y=pai_inc))+
  geom_point(data=Wombat_PAI_inc_byyear,
             aes(x=as.factor(month), y=pai_inc, colour=year))+
  theme_bw()+ 
  labs(x="month", y="absolute dPAI/dt")

p1 / p2

p3 / p4

p1 / p3
