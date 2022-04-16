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
#' @inheritParams moran_bv_calc
#' @keywords internal
local_moran_bv_calc <- function(x, yj, wt) {
  x * mapply(function(wij, yj) sum(wij * yj), wt, yj)
}

# local_moran_bv_calc(scale(x), find_xj(y, nb), wt)

#' Local Bivariate Moran's I conditional permutation implementation
#' @inheritParams moran_bv_perm_impl
#' @keywords internal
local_moran_bv_perm_impl <- function(x, y, listw) {
  p_listw <- permute_listw(listw)

  nb <- p_listw[["neighbours"]]
  wt <- p_listw[["weights"]]

  p_yj <- find_xj(y, nb)

  local_moran_bv_calc(x, p_yj, wt)
}

# local_moran_bv_perm_impl(x, y, listw)
#' Local Bivariate Moran's I spdep implementation
#' @inheritParams moran_bv_impl
#' @keywords internal
local_moran_bv_impl <- function(x, y, listw, nsim) {

  x <- as.numeric(scale(x))
  y <- as.numeric(scale(y))
  nb <- listw[["neighbours"]]
  wt <- listw[["weights"]]

  obs <- local_moran_bv_calc(x, find_xj(y, nb),  wt)
  reps <- replicate(nsim, local_moran_bv_perm_impl(x, y, listw))
  p_sim <- (rowSums(obs <= reps) + 1 )/ (nsim + 1)

  data.frame("Ib" = obs,
       p_sim = pmin(p_sim, 1 - p_sim)
  )
}
#' Compute the Local Bivariate Moran's I Statistic
#'
#' Given two continuous numeric variables, calculate the bivariate Local Moran's I.
#'
#' \eqn{
#' I_i^B= cx_i\Sigma_j{w_{ij}y_j}
#' }
#' @inheritParams global_moran_bv
#' @family global_moran
#' @export
#' @examples
#' x <- guerry_nb$crime_pers
#' y <- guerry_nb$wealth
#' nb <- guerry_nb$nb
#' wt <- guerry_nb$wt
#' local_moran_bv(x, y, nb, wt)
#' @returns a `data.frame` containing two columns `Ib` and `p_sim` containing the local bivariate Moran's I and simulated p-values respectively.
local_moran_bv <- function(x, y, nb, wt, nsim = 499) {
  listw <- recreate_listw(nb, wt)
  local_moran_bv_impl(x, y, listw, nsim = nsim)
}
