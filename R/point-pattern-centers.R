#' Calculate Center Mean Point
#'
#' Given an sfc object, calculates the mean center or weighted mean center of the points.
#'
#' @param geometry an sfc object. If a polygon, uses [`sf::st_point_on_surface()`].
#' @param weights an optional vector of weights to apply to the coordinates before calculation.
#' @family point-pattern
#' @export
center_mean <- function(geometry, weights = NULL) {
  geometry <- check_polygon(geometry)
  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)

  n <- nrow(coords)

  if (!is.null(weights)) {
    res <- colSums(coords * weights) / n
  } else {
    res <- colSums(coords) / n
  }
  sf::st_sfc(sf::st_point(res), crs = crs)
}



#' Calculate Median Center
#'
#' @inheritParams center_mean
#' @family point-pattern
#' @export
center_median <- function(geometry) {
  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)
  res <- apply(coords, 2, median)
  sf::st_sfc(sf::st_point(res), crs = crs)
}



#' Calculate Euclidean Median Center
#'
#' Given an sfc geometry, calculate the Euclidean Median Center.
#'
#' @inheritParams center_mean
#' @param tolerance a tolerance level to terminate the process. This is passed to [`Rfast::spat.med`].
#' @details
#'
#' Calculation of the Euclidean median is done using the [Rfast] package. If Rfast is not available, the function will error.
#' @export
euclidean_median <- function(geometry, tolerance = 1e-09) {
  check_pkg_suggests("pracma")

  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)
  res <- pracma::geo_median(coords)
  sf::st_sfc(sf::st_point(res[["p"]]), crs = crs)
}


# Testing data
# df <- sf::read_sf('/Users/josiahparry/Library/r-miniconda-arm64/envs/geo/lib/python3.8/site-packages/libpysal/examples/virginia/vautm17n_points.shp')
#
# geometry <- sf::st_geometry(df)
# center_mean(geometry)
# center_median(geometry)
# euclidean_median(geometry)
# cent <- euclidean_median(geometry)
# cent_xy <- sf::st_coordinates(cent)
#
# coords <- sf::st_coordinates(geometry)
#
# dists <- sf::st_distance(geometry, cent, by_element = TRUE)
#
# mean(dists)
