rm(list = ls())

library(plotly)
library(ncdf4)

site <- 'Tumbarumba'
site <- 'WallabyCreek'
# site <- 'AliceSpringsMulga'

path_site <- paste0('/Users/30060406/git/lpjguess_dave_assaf/benchmarks/ozflux/',site,'/out/')

setwd(path_site)

lai <- read.table('dave_lai.out', header = TRUE)
cmass_sap <- read.table('dave_cmass_sap.out', header = TRUE)
ltor <- read.table('dave_ltor.out', header = TRUE)
height <- read.table('dave_height.out', header = TRUE)

lai$date <- as.POSIXct(paste(lai$Year, lai$Day + 1, sep = "-"), format = "%Y-%j")
cmass_sap$date <- as.POSIXct(paste(cmass_sap$Year, cmass_sap$Day + 1, sep = "-"), format = "%Y-%j")
ltor$date <- as.POSIXct(paste(ltor$Year, ltor$Day + 1, sep = "-"), format = "%Y-%j")
height$date <-  as.POSIXct(paste(height$Year, height$Day + 1, sep = "-"), format = "%Y-%j")

maxy <- max(lai$total)
miny <- min(lai$TeBE)

plot(lai$date,lai$TeBE,  type = 'l', col ="orange",
     ylab = "lai", xlab = "year" )

plot(cmass_sap$date, cmass_sap$TeBE,  type = 'l', col ="red",
     ylab = "sapwood mass, kgC/m^2",
     xlab = 'year')

# plot(ltor$date, ltor$TeBE, type = 'l', col = 'red', ylim=c(0,1.5))

plot(height$date, height$TeBE, type = 'l', col = "green",
     ylab = "tree height, m",
     xlab = 'year')

### PLOTLY - N INPUT
ninput <- read.table('dave_ninput.out', header = TRUE)
ninput_t1000 <- read.table('dave_ninput_times1000.out', header = TRUE)
ninput$date <- as.POSIXct(paste(ninput$Year, ninput$Day + 1, sep = "-"), format = "%Y-%j")
# plot(ninput$dNO3dep,  type = 'l', col ="blue", ylab = "lai",log = "y" )
# par(new=TRUE)
# plot(ninput$dNH4dep,  type = 'l', col ="black", ylab = "lai",log = "y" )

plot_ly() %>%
  add_lines(x = ninput$date, y = ninput$dNO3dep)  %>%
  add_lines(x = ninput$date, y = ninput$dNH4dep)



# par(new=TRUE)
# plot(lai$date,lai$TeNE,  type = 'l', col="green", ylim = c(miny,maxy), ylab = "lai" )
# par(new=TRUE)
# plot(lai$date,lai$total,  type = 'l', col="black", ylim = c(miny,maxy), ylab = "lai" )
# par(new=TRUE)
# plot(lai$date,lai$total - lai$C3G_perennial,  type = 'l', col ="red", ylim =c(miny,maxy), ylab = "lai" )

