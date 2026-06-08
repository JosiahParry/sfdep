# Calculate neighbor distances

From an nb list and point geometry, return a list of distances for each
observation's neighbors list.

## Usage

``` r
st_nb_dists(x, nb, longlat = NULL)
```

## Arguments

- x:

  an object of class `sfc`.

- nb:

  a neighbor list for example created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)

- longlat:

  `TRUE` if point coordinates are longitude-latitude decimal degrees, in
  which case distances are measured in kilometers. See
  `?spdep::nbdists()` for more.

## Value

a list where each element is a numeric vector.

## Details

Utilizes
[`spdep::nbdists()`](https://r-spatial.github.io/spdep/reference/nbdists.html)
for distance calculation.

## See also

Other weights:
[`st_inverse_distance()`](https://josiahparry.github.io/sfdep/reference/st_inverse_distance.md),
[`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md),
[`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
dists <- st_nb_dists(geo, nb)
#> ! Polygon provided. Using point on surface.

head(dists)
#> [[1]]
#> [1] 90030.63 87399.28 55587.69 85693.43
#> 
#> [[2]]
#> [1]  85221.38  75937.49 125737.35  86797.26 102272.51 105519.50
#> 
#> [[3]]
#> [1]  85460.95  92562.86  96614.50  88010.48  66400.10 111763.74
#> 
#> [[4]]
#> [1] 53294.18 98250.20 83762.06 83973.87
#> 
#> [[5]]
#> [1] 53294.18 86812.56 91755.83
#> 
#> [[6]]
#> [1]  67062.90  96221.69 107959.75 108017.90  54530.02  75976.27 110723.87
#> 
```
