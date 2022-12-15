#' Calculate Center Mean Point
#'
#' Given an sfc object containing points, calculate a measure of central tendency.
#'
#' @param geometry an sfc object. If a polygon, uses [`sf::st_point_on_surface()`].
#' @param weights an optional vector of weights to apply to the coordinates before calculation.
#' @family point-pattern
#' @details
#'
#' - `center_mean()` calculates the mean center of a point pattern
#' - `euclidean_median()` calculates the euclidean median center of a point pattern using the `pracma` package
#' - `center_median()` calculates the median center it is recommended to use the euclidean median over the this function.
#'
#' @returns an sfc POINT object
#' @export
#' @examples
#' # Make a grid to sample from
#' grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))
#'
#' # sample 100 points
#' pnts <- sf::st_sample(grd, 100)
#'
#' cm <- center_mean(pnts)
#' em <- euclidean_median(pnts)
#' cmed <- center_median(pnts)
#'
#' plot(pnts)
#' plot(cm, col = "red", add = TRUE)
#' plot(em, col = "blue", add = TRUE)
#' plot(cmed, col = "green", add = TRUE)
center_mean <- function(geometry, weights = NULL) {
  geometry <- check_polygon(geometry)
  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)

  n <- nrow(coords)

  if (!is.null(weights)) {
    # https://github.com/JosiahParry/sfdep/issues/35
    # shouts out @JoseLastra
    res <- colSums(coords * weights) / sum(weights, na.rm = TRUE)
  } else {
    res <- colSums(coords) / n
  }
  sf::st_sfc(sf::st_point(res), crs = crs)
}


#' @family point-pattern
#' @rdname center_mean
#' @export
center_median <- function(geometry) {
  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)
  res <- apply(coords, 2, stats::median)
  sf::st_sfc(sf::st_point(res), crs = crs)
}



#' @param tolerance a tolerance level to terminate the process. This is passed to [`pracma::geo_median()`].
#' @rdname center_mean
#' @export
euclidean_median <- function(geometry, tolerance = 1e-09) {
  check_pkg_suggests("pracma")

  crs <- sf::st_crs(geometry)
  coords <- sf::st_coordinates(geometry)
  res <- pracma::geo_median(coords)
  sf::st_sfc(sf::st_point(res[["p"]]), crs = crs)
}
