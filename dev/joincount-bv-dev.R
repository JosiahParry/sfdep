library(spdep)
library(sf)

chi <- sf::read_sf("/Users/josiahparry/Downloads/chicago_commpop/chicago_commpop.geojson")
nbsp <- poly2nb(chi)
listw <- nb2listw(nbsp, style = "B")
nb <- listw$neighbours
wt <- listw$weights
z <- chi$popplus
x <- chi$popneg

xj <- find_xj(x, nb)
zj <- find_xj(z, nb)

index <- which(x == 1L)
# BJC  --------------------------------------------------------------------
# this matches pysal Join_Counts_Local_BV
obs <- jc_bjc_calc(x, xj, z, zj, wt)

# p-value only where x == 1 and local join count > 0
index <- which(x == 1L & obs != 0)
reps <- replicate(nsim, jc_bjc_perm_impl(x, z, listw, index))

l <- (rowSums(obs[index] <= reps) + 1)/ (nsim + 1)
pmin(l, 1 -l ) # p-values match pysal


# CLC ---------------------------------------------------------------------






#' Calculate BJC Bivariate Case
#'
#' Assumes no colocation
#' @keywords interna
jc_bjc_calc <- function(x, xj, z, zj, wt) {
  (x * (1 - z)) * mapply(function(wij, zj, xj) sum(wij*zj * (1-xj)),
                         wt, zj, xj)
}

#' Calculate BJC BV for conditional permutations
#' @keywords internal
jc_bjc_perm_impl <- function(x, z, listw, index) {
  p_listw <- permute_listw(listw)
  wt_p <- p_listw[["weights"]][index]
  nb_p <- p_listw[["neighbours"]][index]
  xj_p <- find_xj(x, nb_p)
  zj_p <- find_xj(z, nb_p)
  x_p <- x[index]
  z_p <- z[index]

  jc_bjc_calc(x_p, xj_p, z_p, zj_p, wt_p)

}


# https://geodacenter.github.io/workbook/6d_local_discrete/lab6d.html#co-location-join-count-statistic



