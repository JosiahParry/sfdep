# Local Neighbor Match Test

Implements the Local Neighbor Match Test as described in *Tobler's Law
in a Multivariate World* (Anselin and Li, 2020).

## Usage

``` r
nb_match_test(
  x,
  nb,
  wt = st_weights(nb),
  k = 10,
  nsim = 499,
  scale = TRUE,
  .method = "euclidian",
  .p = 2
)
```

## Arguments

- x:

  a numeric vector or a list of numeric vectors of equal length.

- nb:

  a neighbor list object for example as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- wt:

  a weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

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

a `data.frame` with columns

- `n_shared` (integer): the number of shared neighbors between
  geographic and attribute space

- `nb_matches` (list): matched neighbor indexes. Each element is an
  integer vector of same length as the ith observation of `n_shared`

- `knn_nb` (list): the neighbors in attribute space

- `probability` (numeric): the geometric probability of observing the
  number of matches

- `p_sim` (numeric): a folded simulated p-value

## Examples

``` r

if (requireNamespace("dplyr", quietly = TRUE)) {
library(magrittr)
guerry %>%
  dplyr::transmute(nb = st_knn(geometry, k = 10),
         nmt = nb_match_test(list(crime_pers, literacy, suicides),
                             nb, nsim = 999)) %>%
  tidyr::unnest(nmt)
 }
#> ! Polygon provided. Using point on surface.
#> Simple feature collection with 85 features and 6 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 85 × 7
#>    nb    n_shared nb_matches knn_nb probability p_sim                   geometry
#>    <nb>     <int> <list>     <list>       <dbl> <dbl>             <MULTIPOLYGON>
#>  1 <int>        1 <int [1]>  <int>      0.400   0.671 (((801150 2092615, 800669…
#>  2 <int>        3 <int [3]>  <int>      0.0782  0.096 (((729326 2521619, 729320…
#>  3 <int>        5 <int [5]>  <int>      0.00147 0.003 (((710830 2137350, 711746…
#>  4 <int>        4 <int [4]>  <int>      0.0141  0.018 (((882701 1920024, 882408…
#>  5 <int>        0 <int [0]>  <int>      0.260   0.262 (((886504 1922890, 885733…
#>  6 <int>        0 <int [0]>  <int>      0.260   0.244 (((747008 1925789, 746630…
#>  7 <int>        5 <int [5]>  <int>      0.00147 0.003 (((818893 2514767, 818614…
#>  8 <int>        5 <int [5]>  <int>      0.00147 0.002 (((509103 1747787, 508820…
#>  9 <int>        3 <int [3]>  <int>      0.0782  0.101 (((775400 2345600, 775068…
#> 10 <int>        4 <int [4]>  <int>      0.0141  0.016 (((626230 1810121, 626269…
#> # ℹ 75 more rows
```
