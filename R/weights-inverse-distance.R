#' Calculate inverse distance weights
#'
#' From a neighbor list and sf geometry column, calculate inverse distance weight.
#'
#' @details
#'
#' The inverse distance formula is
#' \eqn{w_{ij} = 1 / d_{ij}^\alpha}
#'
#' @param nb a neighbors list object e.g. created by [`st_knn()`] or [`st_contiguity()`]
#' @param geometry sf geometry
#' @param scale default `100`.a value to scale distances by before exponentiating by `alpha`
#' @param alpha default 1. Set to 2 for gravity weights.
#' @returns a list where each element is a numeric vector
#' @family weights
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' wts <- st_inverse_distance(nb, geo)
#' head(wts, 3)
#' wts <- st_inverse_distance(nb, geo, scale = 10000)
#' head(wts, 3)
#' @export
st_inverse_distance <- function(nb, geometry, scale = 100, alpha = 1) {
  pnts = check_polygon(geometry)

  dists <- spdep::nbdists(nb, pnts)

  lapply(dists, inverse_dist_calc, scale, alpha)
}


#' Calculate inverse distance weights
#'
#' @keywords internal
inverse_dist_calc <- function(.x, scale, alpha) {
  res <- 1 / ((.x/scale)^alpha)
  res[which(is.infinite(res))] <- 0
  res
}
