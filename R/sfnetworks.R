# Two methods:
# 1. for work in a pipe
# 2. for work with basic objects

# edges -------------------------------------------------------------------

#' Convert to an edge lines object
#'
#' Given geometry and neighbor and weights lists, create an edge list `sf` object.
#'
#' @export
#' @examples
st_as_edges <- function(x, ...) {
  UseMethod("st_as_edges")
}

#' @param x object of class `sf` or `sfc`.
#' @param nb a neighbor list. If `x` is class `sf`, the unquote named of the column. If `x` is class `sfc`, an object of class `nb` as created from `st_contiguity()`.
#' @param wt a weights list. If `x` is class `sf`, the unquote named of the column. If `x` is class `sfc`, the weights list itself.
#' @rdname st_as_edges
#' @export
st_as_edges.sf <- function(x, nb, wt) {
  nb <- x[[rlang::ensym(nb)]]
  wt <- x[[rlang::ensym(wt)]]

  listw <- recreate_listw(nb, wt)

  spdep::listw2lines(listw, st_geometry(x)) %>%
    dplyr::rename(from = i, to = j, i = i_ID, j = j_ID)
}

#' @rdname st_as_edges
#' @export
st_as_edges.sfc <- function(x, nb, wt) {
  listw <- recreate_listw(nb, wt)
  spdep::listw2lines(listw, x) %>%
    dplyr::rename(from = i, to = j, i = i_ID, j = j_ID)
}

#edges <- st_as_edges(st_geometry(guerry), guerry_nb$nb, guerry_nb$wt)


# nodes -------------------------------------------------------------------


#' Convert to a node point object
#'
#' Given geometry and a neighbor list, creates an `sf` object to be used as nodes in an [`sfnetworks::sfnetwork()`].
#'
#' @export
st_as_nodes <- function(x, ...) {
  UseMethod("st_as_nodes")
}

#' @inheritParams st_as_edges.sfc
#' @rdname st_as_nodes
#' @export
st_as_nodes.sf <- function(x, nb) {
  # if required packages are missing fail
  check_pkg_suggests(c("vctrs", "dplyr", "sf"))

  nb <- x[[rlang::ensym(nb)]]
  curr_names <- rlang::names2(x)
  new_names <- vctrs::vec_as_names(c("i", curr_names), repair = "universal")
  i_col <- new_names[1]
  # this is based on spdep::nb2lines
  if (inherits(geo, "sfc_MULTIPOLYGON") || "sfc_POLYGON") {
    sf::st_geometry(x) <- sf::st_point_on_surface(sf::st_geometry(x))
  }

  dplyr::mutate(x, "{i_col}" := attr(nb, "region.id"), .before = 1)

}

#' @rdname st_as_nodes
#' @export
st_as_nodes.sfc <- function(x, nb) {
  if (inherits(x, "sfc")) {
    if (!inherits(x, "sfc_POINT")) {
      if (inherits(x, "sfc_POLYGON") || inherits(x,
                                                      "sfc_MULTIPOLYGON"))
        x <- sf::st_point_on_surface(x)
      else stop("Point-conforming geometries required")
    }
  }
  sf::st_as_sf(x) %>%
    dplyr::mutate(i = attr(nb, "region.id"), .before = 1)

}

#
#
# nb <- guerry_nb$nb
# wt <- guerry_nb$wt
# geo <- st_geometry(guerry)
#
# st_as_nodes(geo, nb)
# st_as_edges(geo, nb, wt)
# gg <- st_as_graph(geo, nb, wt)
#


# graph -------------------------------------------------------------------

#' Create an sfnetwork
#'
#' Given an `sf` or `sfc` object and neighbor and weights lists, create an `sfnetwork` object.
#'
#' @export
st_as_graph <- function(x, ...) {
  UseMethod("st_as_graph")
}


#' @inheritParams st_as_edges.sf
#' @rdname st_as_graph
#' @export
st_as_graph.sf <- function(x, nb, wt) {
  check_pkg_suggests("sfnetworks")

  if (!inherits(x, "sf")) rlang::abort("`x` must be an object of `sf` class.")

  nb <- x[[rlang::ensym(nb)]]
  wt <- x[[rlang::ensym(wt)]]

  nodes <- st_as_nodes(x, nb)
  edges <- st_as_edges(x, nb, wt)

  sfnetworks::sfnetwork(nodes, edges)
}

#' @inheritParams st_as_edges.sfc
#' @rdname st_as_graph
#' @export
st_as_graph.sfc <- function(x, nb, wt) {
  if (!inherits(x, "sfc")) rlang::abort("`x` must be an object of class `sf` or `sfc`")

  sfnetworks::sfnetwork(
    st_as_nodes(x, nb),
    st_as_edges(x, nb, wt)
  )
}

# utils -------------------------------------------------------------------

#' Check if a vector of packages are available
#'
#' @param x a character vector of package names
#' @keywords internal
check_pkg_suggests <- function(x) {
  missing_pkgs <- !vapply(x, requireNamespace, FUN.VALUE = logical(1), quietly = TRUE)

  if (any(missing_pkgs))
    cli::cli_abort('Missing packages: {paste("`", x[missing_pkgs], "`", sep = "", collapse = ", ")}')
}

# example -----------------------------------------------------------------
