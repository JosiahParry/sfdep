#' Create node features from edges
#'
#' Given a tidygraph object, create a list column of edge data for each node in the node context.
#'
#'
#' @details
#'
#' - `node_get_nbs()`: creates a neighbor list in the nodes context based on the adjacency list. This returns a `nb` class object with the _neighboring nodes_.
#'   - Uses `igraph::get.adjlist()`
#' - `node_get_edge_list()`: creates an edge list. The edge list contains the row index of the edge relationships in the edge context for each node.
#'   - Uses `igraph::get.adjedgelist()`.
#' - `node_get_edge_col()`: creates a list column containing edge attributes as a list column in the node context (much like `find_xj()`).
#'   - Uses `igraph::get.edge.attribute()`
#'
#' @returns
#'
#' A list column
#'
#' @examples
#'
#' if (interactive()) {
#'   net <- sfnetworks::as_sfnetwork(
#'     sfnetworks::roxel
#'   )
#'
#'   dplyr::mutate(
#'     net,
#'     nb = node_get_nbs(),
#'     edges = node_get_edge_list(),
#'     types = node_get_edge_col(edges, "type")
#'   )
#' }

#' @family sfnetworks
#' @export
node_get_nbs <- function() {
  check_pkg_suggests(c("igraph", "tidygraph"))
  nb <- lapply(igraph::get.adjlist(tidygraph::.G()), as.integer)
  nb <- class_modify(nb, "nb")
  attr(nb, "self.included") <- FALSE
  nb
}

#' @rdname node_get_nbs
#' @family sfnetworks
#' @export
node_get_edge_list <- function() {
  check_pkg_suggests(c("igraph", "tidygraph"))
  lapply(igraph::get.adjedgelist(tidygraph::.G()), as.integer)
}

#' @rdname node_get_nbs
#' @family sfnetworks
#' @param edges an edge list as created by `node_get_edge_list()`
#' @param .var the quoted name of a column in the edge context.
#' @export
node_get_edge_col <- function(edges, .var) {
  check_pkg_suggests(c("igraph", "tidygraph"))
  lapply(
    edges,
    function(e) igraph::get.edge.attribute(tidygraph::.G(), .var, e)
    )

}
