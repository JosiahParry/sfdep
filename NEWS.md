# sfdep 0.2.1

- Bivariate Moran uses the new spdep implementation which is faster. It does, however, return an object of class `boot` which needs extra work to calculate the p-value
- Local join count univariate uses spdep now
- adds graph based neighbors:
  - st_nb_delaunay() uses spdep::tri2nb()
  - st_nb_gabriel() uses spdep::gabrielneigh() and spdep::graph2nb()
  - st_nb_relative() uses spdep::relativeneigh() and spdep::graph2nb()
- `activate()` is now exported as a generic method to no longer conflict with tidygraph
- `st_as_graph()` now creates undirected graphs
- new functions for sfnetworks
  - `node_get_nbs()`: converts the node adjacency list to a neighbor list
  - `node_get_edge_list()`: creates an edge adjacency list used to get edge attribute columns
  - `node_get_edge_col()`: uses the edge list to get edge attributes and turn them into a weight like list

# sfdep 0.2.0

- `spatial_gini()` is a new global measure
- `euclidean_median()` is added for identifying the euclidean median of points for point pattern analysis. 
  - Implemented using `pracma::geo_median()`
    - `pracma` is now added as a suggested package
- `center_mean()` and `center_median()` are added for point pattern analysis
- `tally_jc()` returns join count matrix via `spdep::joincount.multi()` as a data.frame object
- `st_complete_nb()` creates a complete graph of neighbors 
- set operations for neighbor lists `nb_union()`, `nb_intersect()`, `nb_setdiff()`
- `st_block_nb()` is introduced to create neighbor contiguity based on spatial regimes
  - inspired by https://pysal.org/libpysal/generated/libpysal.weights.block_weights.html
- `critical_threshold()` gains argument `k` to identify minimum number of neighbors when calculating threshold
- `pct_nonzero()` calculates the percent of non-zero neighbors
- `szero()` to calculate the global sum of weights
- `nb_as_matrix()` and `wt_as_matrix()` convert neighbor and weights lists to matrices
- two new functions `include_self()` based on `spdep::include.self()` and new `remove_self()` which is available separately in spdep due to [issue 83](https://github.com/r-spatial/spdep/issues/83) as `spdep::remove.self()`. 
- `local_g_perm()` returns correct G statistic. Previously returned only the internal measure.  
- `local_gstar()` and `local_gstar_perm()` do not warn when `self.included = TRUE`
- `emerging_hotspot_analysis()` implements emerging hotspot analysis with spacetime objects
- a new `spacetime` class for linking spatio-temporal data and geometry
  - see the [spacetime vignette](https://sfdep.josiahparry.com/articles/spacetime-s3.html).
- Implemented colocation quotient (CLQ) measures See the [colocation vignette](https://sfdep.josiahparry.com/articles/colocation-analysis.html):
  - `global_colocation()` for global colocation of one categorical variable
  - `pairwise_colocation()` for asymmetric colocation of subcategories of one or two categories
  - `local_colocation()` for local CLQ measure
  - these functions are not fast! PRs welcomed to improve computation speed.
- `st_kernel_weights()` now returns an attribute `kernel` which is set to the value of the kernel argument
- `cond_permute_nb()` specifies `SIMPLIFY = FALSE` in internal `mapply()` call which was causing errors in creating conditionally permuted neighbor lists

# sfdep 0.1.0

* Released on CRAN!
* Sending in for initial CRAN release
