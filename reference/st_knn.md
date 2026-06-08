# Calculate K-Nearest Neighbors

Identifies the `k` nearest neighbors for given point geometry. If
polygon geometry is provided, the centroids of the polygon will be used
and a warning will be emitted.

## Usage

``` r
st_knn(geometry, k = 1, symmetric = FALSE, ...)
```

## Arguments

- geometry:

  an sf or sfc object.

- k:

  number of nearest neighbours to be returned

- symmetric:

  default `FALSE`. Whether to force output of neighbours to be
  symmetric.

- ...:

  additional arguments to be passed to `knearneigh()`.

## Value

a list of class `nb`

## Details

This function utilizes
[`spdep::knearneigh()`](https://r-spatial.github.io/spdep/reference/knearneigh.html)
and
[`spdep::knn2nb()`](https://r-spatial.github.io/spdep/reference/knn2nb.html).

## See also

Other neighbors:
[`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md),
[`st_dist_band()`](https://josiahparry.github.io/sfdep/reference/st_dist_band.md)

## Examples

``` r
st_knn(sf::st_geometry(guerry), k = 8)
#> ! Polygon provided. Using point on surface.
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 680 
#> Percentage nonzero weights: 9.411765 
#> Average number of links: 8 
#> Non-symmetric neighbours list
```
