# Encompassing Higher Order Neighbors

Creates an encompassing neighbor list of the order specified. For
example, if the order is 2 the result contains both 1st and 2nd order
neighbors.

## Usage

``` r
st_nb_lag_cumul(nb, order)
```

## Arguments

- nb:

  A neighbor list object as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- order:

  The order of neighbors.

## Value

a list of class `nb`

## Details

Utilizes
[`spdep::nblag_cumul()`](https://r-spatial.github.io/spdep/reference/nblag.html)

## See also

Other other:
[`st_cardinalties()`](https://josiahparry.github.io/sfdep/reference/st_cardinalties.md),
[`st_nb_lag()`](https://josiahparry.github.io/sfdep/reference/st_nb_lag.md)

## Examples

``` r
nb <- st_contiguity(sf::st_geometry(guerry))
st_nb_lag_cumul(nb, 3)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 2164 
#> Percentage nonzero weights: 29.95156 
#> Average number of links: 25.45882 
```
