# # Bivariate moran i
# x <- scale(guerry$crime_pers)
# y <- scale(guerry$infants)
# nb <- guerry_nb$nb
# wt <- guerry_nb$wt
# denominator <- sum(x^2)
#
# df <- sf::read_sf("/Users/josiahparry/Downloads/natregimes/natregimes.geojson")
#
# x <- scale(df$HR90)
# y <- scale(df$HR80)
# nb <- st_contiguity(df)
# wt <- st_weights(nb)
#
# yj <- find_xj(y, nb)

#' Global Bivariate Moran's I calculation
#'
#' @param x a numeric vector of same length as `nb`.
#' @param yj neighbor values of `y`, same length and lengths as `nb`.
#' @inheritParams recreate_listw
#' @keywords internal
moran_bv_calc <- function(x, yj, wt) {
  numerator <- sum(mapply(function(wij, yj, xi) sum(wij * yj * xi),
                          wt, yj, x))
  denominator <- sum(x^2)

  numerator / denominator
}

# moran_bv_calc(x, yj, nb, wt)
# listw <- recreate_listw(nb, wt)

#' Global Bivariate Moran's I conditional permutation implementation
#' @inheritParams moran_bv_calc
#' @param y a numeric vector of same length as `nb`.
#' @param listw a listw object e.g. as created by [recreate_listw()].
#' @keywords internal
moran_bv_perm_impl <- function(x, y, listw) {
  p_listw <- permute_listw(listw)

  nb <- p_listw[["neighbours"]]
  wt <- p_listw[["weights"]]

  p_yj <- find_xj(y, nb)

  moran_bv_calc(x, p_yj, wt)

}

#moran_bv_perm_impl(x, y, listw)

#' Global Bivariate Moran's I spdep implementation
#'
#' @inheritParams moran_bv_perm_impl
#' @param nsim the number of simulations to run.
#' @keywords internal
moran_bv_impl <- function(x, y, listw, nsim) {

  x <- scale(x)
  y <- scale(y)
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]

  obs <- moran_bv_calc(x, find_xj(y, nb),  wt)
  reps <- replicate(nsim, moran_bv_perm_impl(x, y, listw))
  p_sim <- (sum(obs <= reps) + 1 )/ (nsim + 1)

  list("Ib" = obs,
       p_sim = pmin(p_sim, 1 - p_sim)
       )
}


#' Compute the Global Bivariate Moran's I
#'
#' Given two continuous numeric variables, calculate the bivariate Moran's I.
#'
#' \eqn{
#' I_B = \frac{\Sigma_i(\Sigma_j{w_{ij}y_j\times x_i})}{\Sigma_i{x_i^2}}
#' }
#' @inheritParams moran_bv_impl
#' @inheritParams recreate_listw
#' @family global_moran
#' @export
#' @examples
#' x <- guerry_nb$crime_pers
#' y <- guerry_nb$wealth
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' global_moran_bv(x, y, nb, wt)
#' @returns a named list with two elements `Ib` and `p_sim` containing the bivariate Moran'sI and simulated p-value respectively.
global_moran_bv <- function(x, y, nb, wt, nsim = 99) {
  listw <- recreate_listw(nb, wt)
  moran_bv_impl(x, y, listw, nsim = nsim)
}

