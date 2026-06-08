# Create node features from edges

Given a tidygraph object, create a list column of edge data for each
node in the node context.

## Usage

``` r
node_get_nbs()

node_get_edge_list()

node_get_edge_col(edges, .var)
```

## Arguments

- edges:

  an edge list as created by `node_get_edge_list()`

- .var:

  the quoted name of a column in the edge context.

## Value

A list column

## Details

- `node_get_nbs()`: creates a neighbor list in the nodes context based
  on the adjacency list. This returns a `nb` class object with the
  *neighboring nodes*.

  - Uses
    [`igraph::get.adjlist()`](https://r.igraph.org/reference/get.adjlist.html)

- `node_get_edge_list()`: creates an edge list. The edge list contains
  the row index of the edge relationships in the edge context for each
  node.

  - Uses
    [`igraph::get.adjedgelist()`](https://r.igraph.org/reference/get.adjedgelist.html).

- `node_get_edge_col()`: creates a list column containing edge
  attributes as a list column in the node context (much like
  [`find_xj()`](https://josiahparry.github.io/sfdep/reference/find_xj.md)).

  - Uses
    [`igraph::get.edge.attribute()`](https://r.igraph.org/reference/get.edge.attribute.html)

## Examples

``` r

if (interactive()) {
  net <- sfnetworks::as_sfnetwork(
    sfnetworks::roxel
  )

  dplyr::mutate(
    net,
    nb = node_get_nbs(),
    edges = node_get_edge_list(),
    types = node_get_edge_col(edges, "type")
  )
}
```
