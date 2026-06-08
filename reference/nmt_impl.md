# Implementation of Neighbor Match Test

Implementation of Neighbor Match Test

## Usage

``` r
nmt_impl(x, k, listw, nsim = 199, scale = TRUE, .method = "euclidian", .p = 2)
```

## Arguments

- x:

  a numeric vector or a list of numeric vectors of equal length.

- k:

  the number of neighbors to identify in attribute space. Should be the
  same as number of neighbors provided in
  [st_knn](https://josiahparry.github.io/sfdep/reference/st_knn.md).

- nsim:

  the number of simulations to run for calculating the simulated
  p-value.

- scale:

  default `TRUE`. Whether `x` should be scaled or not. Note that
  measures should be standardized.

- .method:

  default `"euclidian"`. The distance measure passed to
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html).

- .p:

  default 2. The power of Minkowski distance passed to the `p` argument
  in [`stats::dist()`](https://rdrr.io/r/stats/dist.html).

## Value

a data frame containing columns:

- n_shared

- nb_matches

- knn_nb

- probability

- p_sim
