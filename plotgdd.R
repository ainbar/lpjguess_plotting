rm(list = ls())

turnover_leaf <- 0.1
k_chilla <- 0
k_chillb <- 100
k_chillk <- 0.05
phengdd5ramp <- 1 # 0 for evergreens
  
# gdd5 <- seq(0,1000, by=0.01)
gdd5 <- 90

chilldays <- seq(0, 50)

# gdd0 <- (k_chilla + k_chillb * exp(-k_chillk * chilldays))
gdd0 <- lapply(chilldays, function(ch) (k_chilla + k_chillb * exp(-k_chillk * ch)))

# phen <- lapply(gdd5, function(g) min(1.0, (1.0-turnover_leaf) + turnover_leaf * (g - gdd0 / phengdd5ramp)))
phen <- lapply(gdd0, function(g) min(1.0, (1.0-turnover_leaf) + turnover_leaf * (gdd5 - g / phengdd5ramp)))

plot(chilldays, phen, type = "l")

