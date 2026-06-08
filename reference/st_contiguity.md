# Identify polygon neighbors

Given an sf geometry of type `POLYGON` or `MULTIPOLYGON` identify
contiguity based neighbors.

## Usage

``` r
st_contiguity(geometry, queen = TRUE, ...)
```

## Arguments

- geometry:

  an sf or sfc object.

- queen:

  default `TRUE`. For more see
  [`?spdep::poly2nb`](https://r-spatial.github.io/spdep/reference/poly2nb.html)

- ...:

  additional arguments passed to
  [`spdep::poly2nb()`](https://r-spatial.github.io/spdep/reference/poly2nb.html)

## Value

a list of class `nb`

## Details

Utilizes
[`spdep::poly2nb()`](https://r-spatial.github.io/spdep/reference/poly2nb.html)

## See also

Other neighbors:
[`st_dist_band()`](https://josiahparry.github.io/sfdep/reference/st_dist_band.md),
[`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md)

## Examples

``` r
# on basic polygons
geo <- sf::st_geometry(guerry)
st_contiguity(geo)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 420 
#> Percentage nonzero weights: 5.813149 
#> Average number of links: 4.941176 
if (requireNamespace("dplyr", quietyl = TRUE)) {
# in a pipe
library(magrittr)
guerry %>%
  dplyr::mutate(nb = st_contiguity(geometry), .before = 1)
 }
```
