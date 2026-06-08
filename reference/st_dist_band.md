# Neighbors from a distance band

Creates neighbors based on a distance band. By default, creates a
distance band with the maximum distance of k-nearest neighbors where k =
1 (the critical threshold) to ensure that there are no regions that are
missing neighbors.

## Usage

``` r
st_dist_band(geometry, lower = 0, upper = critical_threshold(geometry), ...)
```

## Arguments

- geometry:

  An sf or sfc object.

- lower:

  The lower threshold of the distance band. It is recommended to keep
  this as 0.

- upper:

  The upper threshold of the distance band. By default is set to a
  critical threshold using
  [`critical_threshold()`](https://josiahparry.github.io/sfdep/reference/critical_threshold.md)
  ensuring that each region has a minimum of one neighbor.

- ...:

  Passed to
  [`spdep::dnearneigh()`](https://r-spatial.github.io/spdep/reference/dnearneigh.html).

## Value

a list of class `nb`

## See also

Other neighbors:
[`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md),
[`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
st_dist_band(geo, upper = critical_threshold(geo))
#> ! Polygon provided. Using point on surface.
#> Warning: neighbour object has 24 sub-graphs
#> Warning: neighbour object has 2 sub-graphs
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 272 
#> Percentage nonzero weights: 3.764706 
#> Average number of links: 3.2 
#> 1 region with no links:
#> 31
#> 2 disjoint connected subgraphs
```
