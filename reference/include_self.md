# Includes self in neighbor list

Includes observed region in list of own neighbors. For some neighbor
lists, it is important to include the ith observation (or self) in the
neighbors list, particularly for kernel weights.

## Usage

``` r
include_self(nb)

remove_self(nb)
```

## Arguments

- nb:

  an object of class `nb` e.g. made by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)

## Value

An object of class `nb`.

## Examples

``` r
nb <- st_contiguity(guerry)
self_included <- include_self(nb)
self_included
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 505 
#> Percentage nonzero weights: 6.989619 
#> Average number of links: 5.941176 
remove_self(self_included)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 420 
#> Percentage nonzero weights: 5.813149 
#> Average number of links: 4.941176 
```
