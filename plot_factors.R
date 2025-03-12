max_alpha_repr_factor <- .2
mean_relative_alphas <- seq(0,1,0.01)
alpha_repr_factor <- 1 - (max_alpha_repr_factor / 
                       (1 + exp(20 * (mean_relative_alphas - .6))))
plot(mean_relative_alphas, alpha_repr_factor, type='l')

# // calculate alpha reproduction "tunned down" by the factor that depends on
# // the conditions of the other pools.
# double relative_alpha_repr = 0.0;
# if (indiv.cmass_repr_limit > 0) {
#   relative_alpha_repr = alpha_repr_factor * 
#     divide(indiv.cmass_repr_limit - indiv.cmass_repr, indiv.cmass_repr_limit);
# }

k2 <- 60
k3 <- 0.67
d <- seq(0,0.5,0.001)
h <- k2 * d ^ k3
plot(d, h, type = 'l')


k2 * 0.0097572872530709738 ^ k3



wscal_30 <- seq(0,1,0.00001)
senesc_scal_water <- 4
qwater_senesc <- 0.7
fwater <- senesc_scal_water*(1-wscal_30^qwater_senesc)
plot(wscal_30, fwater, type = 'l')


