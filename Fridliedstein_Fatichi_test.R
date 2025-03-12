###############
## CONSTANTS ##
###############
# wilting point and field capacity, repectively
WP <- 0
FC <- 1
# from T&C: r0 and s0 are coefficients that indicate the theoretically unstressed 
# allocation to leaves, roots, and living sapwood. The coefficients r0 and s0, 
# for woody plants, are a function of the tree biomass, which is typically 
# related to the total stand biomass and tree density (Niklas and Enquist, 2002;
# Wolf et al., 2011b,a).(from T&C documentation)
# comment: 0.2 and 0.4 works.
r0 <- 0.3
s0 <- 0.3
# extinction coeff for allocation function (T&C documentation)
k <- 0.15

##########
# INPUTS #
##########
# relative soil moisture (range 0:1)
SW <- 1
# LAI
LAI <- 4 
# nitrogen coefficient (1 - no deficit; 0 - highest deficit)
FNC <- 1

###########
# FACTORS #
###########
# comment: larger = more "demand"
# light factor (light "demand")
L <- max(0.1, exp(-k * LAI)) 
# soil moisture factoor (1 - no deficit; 0 - highest deficit)
beta <- (SW - WP) / (FC - WP)
# water factor
W <- max(0.15, min(1, beta))
# Nitrogen factor
N <- max(0.1, FNC)
# Below-ground factor
BG <- min(W, N)


########################
# ALLOCATION FRACTIONS #
########################
A_ROOT <- max( 0.15, r0 * ( (3 * L) / (L + (2 * BG)) ) )
A_STEM <- min( 0.75, s0 * ( (3 * BG) / ((2 * L) + BG)) )
A_LEAF <- max( 0.2 , min( 0.5, 1 - A_ROOT - A_STEM ) )

# print factors
print(paste0('light factor... ', round(L, 2), '; Below-ground factor... ', round(BG, 2)))

# print outputs
print(paste0('roots... ', round(A_ROOT,2), 
             '; stem... ', round(A_STEM,2), 
             '; leaves... ', round(A_LEAF,2), 
             '; .... Total is...... ', round(A_ROOT+A_STEM+A_LEAF,2)))
