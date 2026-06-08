# Identify critical threshold

Identifies the minimum distance in which each observation will have at
least one neighbor.

## Usage

``` r
critical_threshold(geometry, k = 1)
```

## Arguments

- geometry:

  an sf geometry column

- k:

  the minimum number of neighbors to check for

## Value

a numeric scalar value.

## Examples

``` r
critical_threshold(sf::st_geometry(guerry))
#> Warning: neighbour object has 24 sub-graphs
#> [1] 94141.96
```
