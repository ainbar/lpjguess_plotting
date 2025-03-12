rel_storage <- seq(0, 1, 0.001)

coeffpie <-  1 - ((1 / (1 + exp(16 * (rel_storage - .7)))))

# saver
saver <- 1 - (1 / (1 + exp(16 * (rel_storage - .7))))
# spender
spender <- 1 - ((1 / (1 + exp(16 * (rel_storage - .2)))))
# middle
middle <- 1 - ((1 / (1 + exp(16 * (rel_storage - 0.5)))))
# 
# plot(rel_storage, saver,
#      type = 'l', col ="blue", ylim = c(0, 1.2), ylab = "factor")
# par(new=TRUE)
# 
# plot(rel_storage, spender,
#      type = 'l', col="red", ylim = c(0, 1.2), ylab = "factor")

data_saver <- data.frame(rel_storage=rel_storage, factor=saver, type="saver" )
data_spender <- data.frame(rel_storage=rel_storage, factor=spender, type="spender" )
data_middle <- data.frame(rel_storage=rel_storage, factor=middle, type="middle" )
data_both <- rbind(data_saver, data_spender, data_middle)

ggplot(data=data_both)+
  geom_line(aes(x=rel_storage, y=factor, color=type))+
  theme_bw()+
  ylab("proportion of NPP+storage subsidy that will go to growth")


######### alpha_repr_factor
# note : when the mean alphas is high, this means that the pools are far from their
# target - hence, the pripority for reproduction is small
mean_relative_alphas <- seq(0, 1, 0.01)
max_alpha_repr_factor <- 0.2
alpha_repr_factor <- (max_alpha_repr_factor / 
                       (1 + exp(20 * (mean_relative_alphas - .25))))
plot(mean_relative_alphas, alpha_repr_factor, type ="l")

########## fwater
senesc_scal_water <- 4
qwater_senesc <- 0.7
wscal30 <- seq(0, 1, 0.01)
fwater <-  senesc_scal_water * ( 1 - wscal30 ^ qwater_senesc)
fwater[fwater>1] <- 1

plot(wscal30, fwater, type ="l")
