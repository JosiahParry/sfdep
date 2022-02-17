# library(spdep)
# library(sf)
#
# chi <- sf::read_sf("/Users/josiahparry/Downloads/chicago_commpop/chicago_commpop.geojson")
# nbsp <- poly2nb(chi)
# listw <- nb2listw(nbsp, style = "B")
# nb <- listw$neighbours
# wt <- listw$weights
# x <- chi$popplus
# z <- chi$popneg
# x + z
#
# xj <- find_xj(x, nb)
# zj <- find_xj(z, nb)
# # 2 cases no co location and co location
# # BJCi=xi(1−zi) ∑_j wj zj(1−xj),
#
# qw <- rgeoda::queen_weights(chi)
# gda <- rgeoda::local_bijoincount(qw, data.frame(x, z))
# # No co-location (BJC) ----------------------------------------------------
# # # Using pysal implementation as reference now
# # array([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0,
# #        0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
# #        0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
# #       0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0])
# # libypysal <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 0, 1, 0, 0, 0, 0,
# #                0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0,
# #                0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0,
# #                0, 1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0)
# x <- as.integer(guerry$donations > 10997)
# nb <- st_contiguity(guerry)
# wt <- st_weights(nb, style = "B")
# # guerry_ds['SELECTED'] = 0
# # guerry_ds.loc[(guerry_ds['Donatns'] > 10997), 'SELECTED'] = 1
#
# # bjc <- (x * (1 - z)) * mapply(function(wij, zj, xj) sum(wij*zj * (1-xj)), wt, zj, xj)
#
#
# #jc_ncl_calc(x, xj, z, zj, wt)
#
# # P-value only reported for i where xi == 1
# index <- which(x == 1)
#
# jc_bjc_calc <- function(x, xj, z, zj, wt) {
#   (x * (1 - z)) * mapply(function(wij, zj, xj) sum(wij*zj * (1-xj)),
#                          wt, zj, xj)
# }
#
#
# jc_bjc_perm_impl <- function(x, z, listw, index) {
#   p_listw <- permute_listw(listw)
#   wt_p <- p_listw[["weights"]][index]
#   nb_p <- p_listw[["neighbours"]][index]
#   xj_p <- find_xj(x, nb_p)
#   zj_p <- find_xj(z, nb_p)
#   x_p <- x[index]
#   z_p <- z[index]
#
#   jc_bjc_calc(x_p, xj_p, z_p, zj_p, wt_p)
#
# }
#
# nsim = 999
# nb <- listw[["neighbours"]]
# wt <- listw[["weights"]]
# xj <- find_xj(x, nb)
# zj <- find_xj(z, nb)
#
# obs <- jc_bjc_calc(x, xj, z, zj, wt)
#
# reps <- replicate(nsim, jc_bjc_perm_impl(x, z, listw, index))
#
# (rowSums(obs[index] >= reps) + 1)/ (nsim + 1)
# (rowSums(obs[index] <= reps) + 1)/ (nsim + 1)
#
#
#
#
# ecdf(reps[2,])(obs[2])
#
#
#
# p_value <- switch(
#   alternative,
#   less = (rowSums(reps <= obs[index]) + 1) / (nsim + 1),
#   greater = (rowSums(reps >= obs[index]) + 1)/ (nsim + 1)
# )
#
#
# jc_bjc_perm_impl(x, z, listw)
#
# # Co-location (CLC) -------------------------------------------------------
#
#
# jc_clc_calc <- function(x, xj, z, zj, wt) {
#   (x * z) * mapply(function(wij, xj, zj) sum(wij * xj, zj), wt, xj, zj)
# }
#
# jc_clc_calc(x, xj, z, zj, wt)
#
# Reduce(`+`, list(x, x))
#
#
