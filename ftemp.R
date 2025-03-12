rm(list = ls())
######
# Arrhenius functions from Parent et al. 2012. Parameters should differ between C3 and C4 plants 
Rgas <- 8.314472
Kelvin <- 273.15 
Tdorm <- 5
A = 6.86e8
# activation energy for growth temperature relationship (kJ mol-1) (Parent & Tardieu 2012)
Ha = 63.3 
# Parameter 'T0' in growth temperature relationship (K) (Parent & Tardieu 2012)
T0 = 30.6
T0 = 20
# Parameter 'A' in growth temperature relationship (K-1) (Parent & Tardieu 2012)
alpha = 1.6
# alpha = 3.5
Ha = 63.3
# T0 = 11

# temperature vector (degC)
Tair <- seq(-5,40,.01)

Ha   <- Ha * 1000   
T0   <- T0 + Kelvin   
Tleaf <- Tair + Kelvin    

# function that calulates ftemp
ftempcalc <- function(Tleaf, A, Ha, Rgas, alpha){
  return( 
    A*(Tleaf * exp(-Ha / (Rgas * Tleaf))) / 
           (1 + (exp(-Ha / (Rgas * Tleaf))) ^ (alpha*(1 - Tleaf / T0))) 
    )
}

  
ftemp <- ftempcalc(Tleaf=Tleaf, A, Ha, Rgas, alpha)
plot(Tair, ftemp, type='l')

# reduce ftemp until the graph crosses the x axis at the Tdorm
Tdorm <- Tdorm + Kelvin
y_Tdorm <- ftempcalc(Tdorm, A, Ha, Rgas, alpha)
ftemp <- ftemp - y_Tdorm
plot(Tair, ftemp, type='l')

# normalise everything to 1 and cap it to be only positive
max_ftemp <- max(ftemp)
ftemp <- ftemp / max_ftemp
# plot(Tair, ftemp)

# set a minimum of 0 to the vector
ftemp[which(ftemp<0)] <- 0
plot(Tair, ftemp, type = 'l')
