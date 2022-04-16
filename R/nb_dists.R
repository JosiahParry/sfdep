#' Calculate neighbor distances
#'
#' From an nb list and point geometry, return a list of distances for each observation's neighbors list.
#'
#' @param x an object of class `sfc`.
#' @param nb a neighbor list for example created by [`st_contiguity()`]
#' @param longlat	`TRUE` if point coordinates are longitude-latitude decimal degrees, in which case distances are measured in kilometers. See `?spdep::nbdists()` for more.
#'
#' @details Utilizes `spdep::nbdists()` for distance calculation.
#' @family weights
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' nb <- st_contiguity(geo)
#' dists <- st_nb_dists(geo, nb)
#'
#' head(dists)
#' @returns a list where each element is a numeric vector.
st_nb_dists <- function(x, nb, longlat = NULL) {
  x <- check_polygon(x)
  class_modify(spdep::nbdists(nb, x, longlat))
}


