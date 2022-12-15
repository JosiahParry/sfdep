# Bivariate local moran
# x <- scale(guerry$suicides)
# y <- scale(guerry$crime_pers)
# nb <- guerry_nb$nb
# wt <- guerry_nb$wt
# yj <- find_xj(y, nb)
# listw <- recreate_listw(nb, wt)
#
# mapply(\(wij, yj) sum(wij*yj), wt, yj) * scale(x)

#' Calculate the Local Bivariate Moran Statistic
#'
#' @inheritParams spdep::moran_bv
#' @keywords internal
local_moran_bv_calc <- function(x, y, nb, wt) {
  x * st_lag(y, nb, wt)
}

# local_moran_bv_calc(scale(x), find_xj(y, nb), wt)

#' Local Bivariate Moran's I conditional permutation implementation
#' @inheritParams spdep::moran_bv
#' @keywords internal
local_moran_bv_perm_impl <- function(x, y, listw) {
  p_listw <- permute_listw(listw)

  nb <- p_listw[["neighbours"]]
  wt <- p_listw[["weights"]]



  local_moran_bv_calc(x, y, nb, wt)
}

# local_moran_bv_perm_impl(x, y, listw)
#' Local Bivariate Moran's I spdep implementation
#' @inheritParams spdep::moran_bv
#' @keywords internal
local_moran_bv_impl <- function(x, y, listw, nsim) {

  x <- as.numeric(scale(x))
  y <- as.numeric(scale(y))
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]

  obs <- local_moran_bv_calc(x, y, nb,  wt)
  reps <- replicate(nsim, local_moran_bv_perm_impl(x, y, listw))
  p_sim <- (rowSums(obs <= reps) + 1 )/ (nsim + 1)

  data.frame("Ib" = obs,
       p_sim = pmin(p_sim, 1 - p_sim)
  )
}
#' Compute the Local Bivariate Moran's I Statistic
#'
#' Given two continuous numeric variables, calculate the bivariate Local Moran's I.
#' @details
#'
#' The Bivariate Local Moran, like its global counterpart, evaluates the value
#' of x at observation i with its spatial neighbors' value of y. The value of \deqn{I_i^B} is just xi * Wyi. Or, in simpler words the local bivariate Moran is the result of multiplying x by the spatial lag of y. Formally it is defined as
#'
#' \eqn{
#' I_i^B= cx_i\Sigma_j{w_{ij}y_j}
#' }
#'
#' @inheritParams recreate_listw
#' @inheritParams spdep::moran_bv
#' @family global_moran
#' @export
#' @examples
#' x <- guerry_nb$crime_pers
#' y <- guerry_nb$wealth
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' local_moran_bv(x, y, nb, wt)
#' @returns a `data.frame` containing two columns `Ib` and `p_sim` containing the local bivariate Moran's I and simulated p-values respectively.
#'
#' @references
#' [Local Spatial Autocorrelation (3): Multivariate Local Spatial Autocorrelation, Luc Anselin](https://geodacenter.github.io/workbook/6c_local_multi/lab6c.html#principle)
local_moran_bv <- function(x, y, nb, wt, nsim = 499) {
  listw <- recreate_listw(nb, wt)
  local_moran_bv_impl(x, y, listw, nsim = nsim)
}
