# Calculate inverse distance weights

From a neighbor list and sf geometry column, calculate inverse distance
weight.

## Usage

``` r
st_inverse_distance(nb, geometry, scale = 100, alpha = 1)
```

## Arguments

- nb:

  a neighbors list object e.g. created by
  [`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md)
  or
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)

- geometry:

  sf geometry

- scale:

  default `100`.a value to scale distances by before exponentiating by
  `alpha`

- alpha:

  default 1. Set to 2 for gravity weights.

## Value

a list where each element is a numeric vector

## Details

The inverse distance formula is \\w\_{ij} = 1 / d\_{ij}^\alpha\\

## See also

Other weights:
[`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md),
[`st_nb_dists()`](https://josiahparry.github.io/sfdep/reference/st_nb_dists.md),
[`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wts <- st_inverse_distance(nb, geo)
#> ! Polygon provided. Using point on surface.
head(wts, 3)
#> [[1]]
#> [1] 0.001110733 0.001144174 0.001798959 0.001166951
#> 
#> [[2]]
#> [1] 0.0011734145 0.0013168726 0.0007953086 0.0011521100 0.0009777799
#> [6] 0.0009476921
#> 
#> [[3]]
#> [1] 0.0011701250 0.0010803469 0.0010350414 0.0011362283 0.0015060218
#> [6] 0.0008947445
#> 
wts <- st_inverse_distance(nb, geo, scale = 10000)
#> ! Polygon provided. Using point on surface.
head(wts, 3)
#> [[1]]
#> [1] 0.1110733 0.1144174 0.1798959 0.1166951
#> 
#> [[2]]
#> [1] 0.11734145 0.13168726 0.07953086 0.11521100 0.09777799 0.09476921
#> 
#> [[3]]
#> [1] 0.11701250 0.10803469 0.10350414 0.11362283 0.15060218 0.08947445
#> 
```
