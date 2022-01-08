#' Calculate spatial lag
#'
#' Calculates the spatial lag of a numeric variable given a neighbor and weights list.
#'
#' @param x A numeric vector
#' @param nb A neighbor list object as created by `st_neighbors()`.
#' @param wt A weights list as created by `st_weights()`.
#' @param allow_zero If `TRUE`, assigns zero as lagged value to zone without neighbors.
#' @param na_ok Default `FALSE`. If, `TRUE` missing values return a lagged `NA`.
#' @param ... See `?spdep::lag.listw` for more.
#' @family stats
#' @export
st_lag <- function(x, nb, wt, na_ok = FALSE, allow_zero = NULL, ...) {
  listw <- recreate_listw(nb, wt)
  spdep::lag.listw(listw, x, NAOK = na_ok, zero.policy = allow_zero, ...)
}
