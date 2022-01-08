#' Calculate neighbor distances
#'
#' From an nb list and point geometry, return a list of distances for each observation's neighbors list.
#'
#' @inheritParams st_inverse_weights
#' @param longlat	`TRUE` if point coordinates are longitude-latitude decimal degrees, in which case distances are measured in kilometers. See `?spdep::nbdists()` for more.
#'
#' @details Utilizes `spdep::nbdists()` for distance calculation.
#' @family stats
#' @export
st_nb_dists <- function(x, nb, longlat = NULL) {
  spdep::nbdists(nb, x, longlat)
}


