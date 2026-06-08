# Changelog

## sfdep (development version)

- Fixes bug with cli and closes
  [\#49](https://github.com/josiahparry/sfdep/issues/49)

## sfdep 0.2.4

CRAN release: 2024-02-20

- Dexter Locke is now the maintainer of sfdep
- Address failing CRAN checks h/t
  [@rsbivand](https://github.com/rsbivand)
  <https://github.com/JosiahParry/sfdep/pull/43>
- Address bug in local Gi/\* statistics

## sfdep 0.2.3

CRAN release: 2023-01-11

- remove readr suggestion to pass `noSuggests` from CRAN. Much minuscule
  tasks for the overlords.

## sfdep 0.2.2

CRAN release: 2022-12-22

- no changes from 0.2.1 version bump solely for CRAN

## sfdep 0.2.1

CRAN release: 2022-12-16

- Bivariate Moran uses the new spdep implementation which is faster. It
  does, however, return an object of class `boot` which needs extra work
  to calculate the p-value
- Local join count univariate uses spdep now
- adds graph based neighbors:
  - st_nb_delaunay() uses spdep::tri2nb()
  - st_nb_gabriel() uses spdep::gabrielneigh() and spdep::graph2nb()
  - st_nb_relative() uses spdep::relativeneigh() and spdep::graph2nb()
- [`activate()`](https://josiahparry.github.io/sfdep/reference/activate.md)
  is now exported as a generic method to no longer conflict with
  tidygraph
- [`st_as_graph()`](https://josiahparry.github.io/sfdep/reference/st_as_graph.md)
  now creates undirected graphs
- new functions for sfnetworks
  - [`node_get_nbs()`](https://josiahparry.github.io/sfdep/reference/node_get_nbs.md):
    converts the node adjacency list to a neighbor list
  - [`node_get_edge_list()`](https://josiahparry.github.io/sfdep/reference/node_get_nbs.md):
    creates an edge adjacency list used to get edge attribute columns
  - [`node_get_edge_col()`](https://josiahparry.github.io/sfdep/reference/node_get_nbs.md):
    uses the edge list to get edge attributes and turn them into a
    weight like list

## sfdep 0.2.0

CRAN release: 2022-08-15

- [`spatial_gini()`](https://josiahparry.github.io/sfdep/reference/spatial_gini.md)
  is a new global measure
- [`euclidean_median()`](https://josiahparry.github.io/sfdep/reference/center_mean.md)
  is added for identifying the euclidean median of points for point
  pattern analysis.
  - Implemented using
    [`pracma::geo_median()`](https://rdrr.io/pkg/pracma/man/geo_median.html)
    - `pracma` is now added as a suggested package
- [`center_mean()`](https://josiahparry.github.io/sfdep/reference/center_mean.md)
  and
  [`center_median()`](https://josiahparry.github.io/sfdep/reference/center_mean.md)
  are added for point pattern analysis
- [`tally_jc()`](https://josiahparry.github.io/sfdep/reference/global_jc_perm.md)
  returns join count matrix via
  [`spdep::joincount.multi()`](https://r-spatial.github.io/spdep/reference/joincount.multi.html)
  as a data.frame object
- [`st_complete_nb()`](https://josiahparry.github.io/sfdep/reference/st_complete_nb.md)
  creates a complete graph of neighbors
- set operations for neighbor lists
  [`nb_union()`](https://josiahparry.github.io/sfdep/reference/nb_union.md),
  [`nb_intersect()`](https://josiahparry.github.io/sfdep/reference/nb_union.md),
  [`nb_setdiff()`](https://josiahparry.github.io/sfdep/reference/nb_union.md)
- [`st_block_nb()`](https://josiahparry.github.io/sfdep/reference/st_block_nb.md)
  is introduced to create neighbor contiguity based on spatial regimes
  - inspired by
    <https://pysal.org/libpysal/generated/libpysal.weights.block_weights.html>
- [`critical_threshold()`](https://josiahparry.github.io/sfdep/reference/critical_threshold.md)
  gains argument `k` to identify minimum number of neighbors when
  calculating threshold
- [`pct_nonzero()`](https://josiahparry.github.io/sfdep/reference/pct_nonzero.md)
  calculates the percent of non-zero neighbors
- [`szero()`](https://josiahparry.github.io/sfdep/reference/szero.md) to
  calculate the global sum of weights
- [`nb_as_matrix()`](https://josiahparry.github.io/sfdep/reference/wt_as_matrix.md)
  and
  [`wt_as_matrix()`](https://josiahparry.github.io/sfdep/reference/wt_as_matrix.md)
  convert neighbor and weights lists to matrices
- two new functions
  [`include_self()`](https://josiahparry.github.io/sfdep/reference/include_self.md)
  based on
  [`spdep::include.self()`](https://r-spatial.github.io/spdep/reference/include.self.html)
  and new
  [`remove_self()`](https://josiahparry.github.io/sfdep/reference/include_self.md)
  which is available separately in spdep due to [issue
  83](https://github.com/r-spatial/spdep/issues/83) as
  [`spdep::remove.self()`](https://r-spatial.github.io/spdep/reference/include.self.html).
- [`local_g_perm()`](https://josiahparry.github.io/sfdep/reference/local_g.md)
  returns correct G statistic. Previously returned only the internal
  measure.  
- [`local_gstar()`](https://josiahparry.github.io/sfdep/reference/local_gstar.md)
  and
  [`local_gstar_perm()`](https://josiahparry.github.io/sfdep/reference/local_gstar.md)
  do not warn when `self.included = TRUE`
- [`emerging_hotspot_analysis()`](https://josiahparry.github.io/sfdep/reference/emerging_hotspot_analysis.md)
  implements emerging hotspot analysis with spacetime objects
- a new `spacetime` class for linking spatio-temporal data and geometry
  - see the [spacetime
    vignette](https://sfdep.josiahparry.com/articles/spacetime-s3.html).
- Implemented colocation quotient (CLQ) measures See the [colocation
  vignette](https://sfdep.josiahparry.com/articles/colocation-analysis.html):
  - [`global_colocation()`](https://josiahparry.github.io/sfdep/reference/global_colocation.md)
    for global colocation of one categorical variable
  - [`pairwise_colocation()`](https://josiahparry.github.io/sfdep/reference/pairwise_colocation.md)
    for asymmetric colocation of subcategories of one or two categories
  - [`local_colocation()`](https://josiahparry.github.io/sfdep/reference/local_colocation.md)
    for local CLQ measure
  - these functions are not fast! PRs welcomed to improve computation
    speed.
- [`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md)
  now returns an attribute `kernel` which is set to the value of the
  kernel argument
- [`cond_permute_nb()`](https://josiahparry.github.io/sfdep/reference/cond_permute_nb.md)
  specifies `SIMPLIFY = FALSE` in internal
  [`mapply()`](https://rdrr.io/r/base/mapply.html) call which was
  causing errors in creating conditionally permuted neighbor lists

## sfdep 0.1.0

CRAN release: 2022-04-20

- Released on CRAN!
- Sending in for initial CRAN release
