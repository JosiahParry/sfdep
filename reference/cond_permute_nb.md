# Conditional permutation of neighbors

Creates a conditional permutation of neighbors list holding i fixed and
shuffling it's neighbors.

## Usage

``` r
cond_permute_nb(nb, seed = NULL)
```

## Arguments

- nb:

  a neighbor list.

- seed:

  default null. A value to pass to
  [`set.seed()`](https://rdrr.io/r/base/Random.html) for
  reproducibility.

## Value

A list of class `nb` where each element contains a random sample of
neighbors excluding the observed region.

## Examples

``` r
nb <- st_contiguity(guerry)
nb[1:5]
#> [[1]]
#> [1] 36 37 67 69
#> 
#> [[2]]
#> [1]  7 49 57 58 73 76
#> 
#> [[3]]
#> [1] 17 21 40 56 61 69
#> 
#> [[4]]
#> [1]  5 24 79 80
#> 
#> [[5]]
#> [1]  4 24 36
#> 
# conditionally permute neighbors
perm_nb <- cond_permute_nb(nb)
perm_nb[1:5]
#> [[1]]
#> [1] 33 79 76 77
#> 
#> [[2]]
#> [1] 81 42 79 12 58 80
#> 
#> [[3]]
#> [1] 29  2 17 52 55 77
#> 
#> [[4]]
#> [1] 57  6  1 45
#> 
#> [[5]]
#> [1] 77  3 16
#> 
```
