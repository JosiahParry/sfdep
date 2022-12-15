#' Compute the Global Bivariate Moran's I
#'
#' Given two continuous numeric variables, calculate the bivariate Moran's I. See details for more.
#'
#'@details
#'
#' The Global Bivariate Moran is defined as
#'
#' \eqn{
#' I_B = \frac{\Sigma_i(\Sigma_j{w_{ij}y_j\times x_i})}{\Sigma_i{x_i^2}}
#' }
#'
#' It is important to note that this is a measure of autocorrelation of X
#' with the spatial lag of Y. As such, the resultant measure may overestimate the amount of
#' spatial autocorrelation which may be a product of the inherent correlation of X and Y.
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
#' global_moran_bv(x, y, nb, wt)
#' @returns an object of class boot
#'
#' @references
#' [Global Spatial Autocorrelation (2): Bivariate, Differential and EB Rate Moran Scatter Plot, Luc Anselin](https://geodacenter.github.io/workbook/5b_global_adv/lab5b.html)
#'
global_moran_bv <- function(x, y, nb, wt, nsim = 99, scale = TRUE) {
  listw <- recreate_listw(nb, wt)
  spdep::moran_bv(x, y, listw, nsim, scale)
}

