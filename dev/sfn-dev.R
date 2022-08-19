
#' @export
node_get_nbs <- function() {
  lapply(igraph::get.adjlist(tidygraph::.G()), as.integer)
}

#' @export
node_get_edge_list <- function() {
  lapply(igraph::get.adjedgelist(tidygraph::.G()), as.integer)
}

#' @export
node_get_edge_col <- function(edges, .var) {
  lapply(edges,
         function(e) igraph::get.edge.attribute(net, .var, e)
         )

}



#
# library(sfnetworks)
#
# net <- as_sfnetwork(roxel)
#
# res <- dplyr::mutate(
#   net,
#   nb = node_get_nbs(),
#   elist = node_get_edge_list(),
#   types = node_get_edge_col(elist, "type")
# )
