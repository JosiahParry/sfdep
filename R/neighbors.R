#' Identify polygon neighbors
#'
#' Given an sf geometry of type `POLYGON` or `MULTIPOLYGON` identify contiguity based neighbors.
#'
#' @details
#' Utilizes [`spdep::poly2nb()`]
#' @param x an sf or sfc object.
#' @param queen default `TRUE`. For more see `?spdep::poly2nb`
#' @param ... additional arguments passed to [spdep::poly2nb()]
#' @importFrom spdep poly2nb
#' @family neighbors
#' @export
#' @examples
#' # on basic polygons
#' geo <- sf::st_geometry(guerry)
#' st_contiguity(geo)
#'
#' # in a pipe
#' library(magrittr)
#' guerry %>%
#'   dplyr::mutate(nb = st_contiguity(geometry), .before = 1)
#' @returns a list of class `nb`
st_contiguity <- function(x, queen = TRUE, ...) {
  nb <- spdep::poly2nb(x, queen = queen, ...)
  class_modify(nb)
}


#' Calculate K-Nearest Neighbors
#'
#' Identifies the `k` nearest neighbors for given point geometry. If polygon geometry is provided, the centroids of the polygon will be used and a warning will be emitted.
#'
#' @details
#'
#' This function utilizes [spdep::knearneigh()] and [spdep::knn2nb()].
#'
#' @param x an sf or sfc object.
#' @param k number of nearest neighbours to be returned
#' @param symmetric default `FALSE`. Whether to force output of neighbours to be symmetric.
#' @param ... additional arguments to be passed to `knearneigh()`.
#' @importFrom sf st_centroid st_geometry
#' @importFrom spdep knn2nb knearneigh
#' @family neighbors
#' @export
#' @examples
#' st_knn(sf::st_geometry(guerry), k = 8)
#' @returns a list of class `nb`
st_knn <- function(x, k = 1, symmetric = FALSE, ...) {

  pnts <- check_polygon(x)

  suppressWarnings({
    ks <- spdep::knearneigh(pnts, k = k, ...)
    nb <- spdep::knn2nb(ks, sym = symmetric)
  })


  class_modify(nb)
}

#' Neighbors from a distance band
#'
#' Creates neighbors based on a distance band. By default, creates a distance band with the maximum distance of k-nearest neighbors where k = 1 (the critical threshold) to ensure that there are no regions that are missing neighbors.
#'
#' @param geometry An sf or sfc object.
#' @param lower The lower threshold of the distance band. It is recommended to keep this as 0.
#' @param upper The upper threshold of the distance band. By default is set to a critical threshold using [`critical_threshold()`] ensuring that each region has a minimum of one neighbor.
#' @param ... Passed to `spdep::dnearneigh()`.
#' @family neighbors
#' @export
#' @examples
#' geo <- sf::st_geometry(guerry)
#' st_dist_band(geo, upper = critical_threshold(geo))
#' @returns a list of class `nb`
st_dist_band <- function(geometry, lower = 0,
                         upper = critical_threshold(geometry), ...) {
  x <- check_polygon(geometry)
  class_modify(spdep::dnearneigh(x, lower, upper, ...))
}


#' Pure Higher Order Neighbors
#'
#' Identify higher order neighbors from a neighbor list. `order` must be greater than 1. When order equals 2 then the neighbors of the neighbors list is returned and so forth. See [Anselin's slides](https://spatial.uchicago.edu/sites/spatial.uchicago.edu/files/1_introandreview_reducedsize.pdf) for an example.
#'
#' @details
#' Utilizes [`spdep::nblag()`]
#' @param nb A neighbor list object as created by `st_contiguity()`.
#' @param order The order of neighbors.
#' @importFrom spdep nblag
#' @family other
#' @export
#' @examples
#' nb <- st_contiguity(sf::st_geometry(guerry))
#' st_nb_lag(nb, 3)
#' @returns a list of class `nb`
st_nb_lag <- function(nb, order) {
  class_modify(nblag(nb, order)[[order]])
}


#' Encompassing Higher Order Neighbors
#'
#' Creates an encompassing neighbor list of the order specified.
#' For example, if the order is 2 the result contains both 1st
#' and 2nd order neighbors.
#'
#' @details
#' Utilizes [`spdep::nblag_cumul()`]
#' @inheritParams st_nb_lag
#' @importFrom spdep nblag_cumul nblag
#' @family other
#' @export
#' @examples
#' nb <- st_contiguity(sf::st_geometry(guerry))
#' st_nb_lag_cumul(nb, 3)
#' @returns a list of class `nb`
st_nb_lag_cumul <- function(nb, order) {
  class_modify(nblag_cumul(nblag(nb, order)))
}


#' Checks geometry for polygons.
#'
#' If the provided geometry is a polygon, a point will be generated using [`sf::st_point_on_surface()`]. If a centroid is preferred, a new column can be created that contains the output of [`sf::st_centroid()`].
#' @param x an sfc object
#' @keywords internal
check_polygon <- function(x) {

  polygon_check <- any(class(x) %in% c("sfc_MULTIPOLYGON", "sfc_POLYGON"))

  if (polygon_check) {

    cli::cli_alert_warning("Polygon provided. Using point on surface.")
    return(sf::st_point_on_surface(x))
  }
  x
}

