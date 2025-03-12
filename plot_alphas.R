rm(list = ls())

setwd('/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux/Tumbarumba/out/')

alpha_leaf <- read.table('dave_dalpha_leaf.out', header = TRUE)
alpha_root <- read.table('dave_dalpha_root.out', header = TRUE)
alpha_sap <- read.table('dave_dalpha_sap.out', header = TRUE)
alpha_repr <- read.table('dave_dalpha_repr.out', header = TRUE)

alpha_leaf$date <- as.POSIXct(paste(alpha_leaf$Year, alpha_leaf$Day + 1, sep = "-"),
                       format = "%Y-%j")
alpha_root$date <- as.POSIXct(paste(alpha_root$Year, alpha_root$Day + 1, sep = "-"),
                              format = "%Y-%j")
alpha_sap$date <- as.POSIXct(paste(alpha_sap$Year, alpha_sap$Day + 1, sep = "-"),
                              format = "%Y-%j")
alpha_repr$date <- as.POSIXct(paste(alpha_repr$Year, alpha_repr$Day + 1, sep = "-"),
                             format = "%Y-%j")

# create dataframe of the data + the sum of all the alphas
alpha_sum <- data.frame(date = alpha_leaf$date)
alpha_sum$alpha_leaf <- alpha_leaf$TeBE
alpha_sum$alpha_root <- alpha_root$TeBE
alpha_sum$alpha_sap <- alpha_sap$TeBE
alpha_sum$alpha_repr <- alpha_repr$TeBE
alpha_sum$alpha_sum <- rowSums(alpha_sum[,2:5])

plot(alpha_sum$date, 
     alpha_sum$alpha_sum,
     type = 'l', col ="black", ylim =c(0,2), ylab = "alpha" )
plot(alpha_sum$date, 
     alpha_sum$alpha_leaf,
     type = 'l', col ="green", ylim =c(0,2), ylab = "alpha" )
plot(alpha_sum$date, 
     alpha_sum$alpha_root,
     type = 'l', col ="yellow", ylim =c(0,2), ylab = "alpha" )

ggplot(data = alpha_sum[1:10000,])+
  geom_line(aes(x = date, y = alpha_leaf , col = "green"))+
  geom_line(aes(x = date, y = alpha_root ,col  = "yellow"))+
  geom_line(aes(x = date, y = alpha_sap  , col = "blue"))+
  geom_line(aes(x = date, y = alpha_repr , col = "red"))
