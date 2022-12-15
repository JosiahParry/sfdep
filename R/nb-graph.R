#' Graph based neighbors
#'
#' Create graph based neighbors on a set of points.
#'
#' @param geometry an object of class sfc. If polygons are used, points are generated using `sf::st_point_on_surface()`.
#' @param .nnmult default 3. Used for memory scalling. See [spdep::gabrielneigh()] for more.
#' @param .id default `NULL`. Passed as `spdep::tri2nb(x, row.names = .id)` to `spdep`.
#'
#' @details
#'
#' - `st_nb_delaunay()` uses `spdep::tri2nb()`
#' - `st_nb_gabriel()` uses `spdep::gabrielneigh()` and `spdep::graph2nb()`
#' - `st_nb_relative()` uses `spdep::relativeneigh()` and `spdep::graph2nb()`
#'
#'  `st_nb_delaunay()` implements Delaunay triangulation via `spdep` and thus via `deldir`. Delaunay triangulation creates a mesh of triangles that connects all points in a set. It ensures that no point is in in the circumcircle of an triangle in the triangulation. As a result, Delaunay triangulation maximizes the minimum angle in each triangle consequently avoiding skinny triangles.
#'
#' The Gabriel graph is a subgraph of the Delaunay triangulation. Edges are created when the closed disc between two points p, and q, contain no other points besides themselves.
#'
#'  The relative neighborhood graph (RNG) is based on the Delaunay triangulation. It connects two points when there are no other closer points to each of them. The RNG is a subgraph of the Delaunay triangulation.
#'
#' Note that Delaunay triangulation assumes a plane and thus uses Euclidean distances.
#'
#' See [spdep::gabrielneigh()] for further descriptions of the graph neighbor implementations.
#'
#'
#' @examples
#' geometry <- sf::st_centroid(sf::st_geometry(guerry))
#' st_nb_delaunay(geometry)
#' st_nb_gabriel(geometry)
#' st_nb_relative(geometry)
#' @export
st_nb_delaunay <- function(geometry, .id = NULL) {
  if (!inherits(geometry, "sfc")) rlang::abort("`geometry` must be class `sfc`.")
  geo <- check_polygon(geometry)
  spdep::tri2nb(geo, .id)
}

#' @rdname st_nb_delaunay
#' @export
st_nb_gabriel <- function(geometry, .nnmult = 3) {
  if (!inherits(geometry, "sfc")) rlang::abort("`geometry` must be class `sfc`.")
  geo <- check_polygon(geometry)
  gneigh <- spdep::gabrielneigh(geo, nnmult = .nnmult)
  spdep::graph2nb(gneigh)
}

#' @rdname st_nb_delaunay
#' @export
#' @returns an object of class nb
st_nb_relative <- function(geometry, .nnmult = 3) {
  if (!inherits(geometry, "sfc")) rlang::abort("`geometry` must be class `sfc`.")
  geo <- check_polygon(geometry)
  r_nb <- spdep::relativeneigh(geo, nnmult = .nnmult)
  spdep::graph2nb(r_nb)
}

