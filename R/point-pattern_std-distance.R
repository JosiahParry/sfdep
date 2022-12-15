#' Calculate standard distance
#'
#' The standard distance of a point pattern is a measure of central tendency.
#' Standard distance measures distance away from the mean center of the point pattern
#' similar to standard deviations.
#'
#'
#' @family point-pattern
#' @inheritParams center_mean
#' @export
#' @examples
#' # Make a grid to sample from
#' grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))
#'
#' # sample 100 points
#' pnts <- sf::st_sample(grd, 100)
#'
#' # plot points
#' plot(pnts)
#'
#' # calculate standard distance
#' std_distance(pnts)
#' @returns
#' A numeric scalar.
std_distance <- function(geometry) {
  geometry <- sf::st_geometry(geometry)
  cent <- center_mean(geometry)
  cent_xy <- sf::st_coordinates(cent)
  xy <- sf::st_coordinates(geometry)
  n <- length(geometry)
  lhs <- sum((xy[,1] - cent_xy[1])^2) / n
  rhs <- sum(((xy[,2] - cent_xy[2])^2)) / n
  sqrt(lhs + rhs)

}




# Example data i used for validation from
# http://pysal.org/notebooks/explore/pointpats/centrography.html#Dispersion-and-Orientation
# library(sf)
# readr::read_table(
#   col_names = c("x", "y"),
#   "66.22	32.54
# 22.52	22.39
# 31.01	81.21
# 9.47	31.02
# 30.78	60.10
# 75.21	58.93
# 79.26	7.68
# 8.23	39.96
# 98.73	77.17
# 89.78	42.53
# 65.19	92.08
# 54.46	8.48") |>
#   st_as_sf(coords = 1:2) -> pnts
#
# cent <- euclidean_median(pnts)
# std_distance(st_geometry(pnts))
