# This function create ellipses directly from
# values
# Venn diagram fx definition
# Function taken from ggVennDiagram

#' Create an Ellipse
#'
#' Generate an ellipse from center coordinates, major and minor axis radii, and angle rotation.
#'
#' @param geometry an sf `ST_POINT` geometry. Can be `sfg`, `sfc`, or `sf` object
#' @param x longitude of center point
#' @param y latitude of center point
#' @param sx radius of major axis
#' @param sy radius of minor axis
#' @param rotation the degree of rotation of the ellipse
#' @param n the number of coordinates to generate for the ellipse
#'
#' @details
#'
#' [ellipse()] returns a matrix of point locations defining the ellipse. [st_ellipse()] returns an sf object with LINE geography of the ellipse. Increasing n increases the number of points generated to define the ellipse shape.
#'
#' [ellipse()] function is adapted from `ggVennDiagram`.
#'
#' @export
ellipse <- function (x = 0, y = 0, sx = 2, sy = 1, rotation = 0, n = 100) {
  rotation <- rotation * pi/180
  theta <- 2 * pi/n
  angles <- seq(0, 2 * pi, theta)
  x_coord <- vector(length = n + 1, mode = "numeric")
  y_coord <- vector(length = n + 1, mode = "numeric")
  for (i in 1:n) {
    x_coord[i] <- x + sx* cos(angles[i]) * cos(rotation) -
      sy * sin(angles[i]) * sin(rotation)
    y_coord[i] <- y + sx * cos(angles[i]) * sin(rotation) +
      sy * sin(angles[i]) * cos(rotation)
  }
  x_coord[n + 1] <- x_coord[1]
  y_coord[n + 1] <- y_coord[1]
  as.matrix(data.frame(x = x_coord, y = y_coord))
}

# ellipse(cent_xy[1], cent_xy[2],
#         sigmax, sigmay, -theta) |>
#   sf::st_linestring() |>
#   sf::st_sfc(crs = crs) |>
#   sf::st_as_sf()

# This function takes an sf object and creates an ellipses
#' @export
#' @rdname ellipse
#' @examples
#' ellipse(n = 10)
#' st_ellipse(sf::st_point(c(0, 0)), sx = 10, sy = 10)
st_ellipse <- function(geometry, sx, sy, rotation = 0, n = 100) {
  crs <- sf::st_crs(geometry)
  cent_xy <- as.numeric(sf::st_coordinates(geometry))
  elip <- ellipse(cent_xy[1], cent_xy[2], sx, sy, rotation, n)
  res <- sf::st_sfc(sf::st_linestring(elip), crs = crs)
  res
}
