# Set Operations

Perform set operations element-wise on two lists of equal length.

## Usage

``` r
nb_union(x, y)

nb_intersect(x, y)

nb_setdiff(x, y)
```

## Arguments

- x:

  list of class `nb`

- y:

  list of class `nb`

## Value

A list of class `nb`

## Details

- `nb_union()` returns the union of elements in each element of x and y

- `nb_intersect()` returns the intersection of elements in each element
  of x and y

- `nb_setdiff()` returns the difference of elements in each element of x
  and y

## Examples

``` r
nb <- st_contiguity(guerry$geometry)
nb_knn <- st_knn(guerry$geometry, k = 3)
#> ! Polygon provided. Using point on surface.
nb_setdiff(nb, nb_knn)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 176 
#> Percentage nonzero weights: 2.435986 
#> Average number of links: 2.070588 
#> 16 regions with no links:
#> 5, 7, 8, 13, 20, 23, 27, 55, 57, 60, 62, 63, 64, 66, 71, 79
#> 10 disjoint connected subgraphs
#> Non-symmetric neighbours list
nb_union(nb, nb_knn)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 431 
#> Percentage nonzero weights: 5.965398 
#> Average number of links: 5.070588 
#> Non-symmetric neighbours list
nb_intersect(nb, nb_knn)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 244 
#> Percentage nonzero weights: 3.377163 
#> Average number of links: 2.870588 
#> Non-symmetric neighbours list
```
