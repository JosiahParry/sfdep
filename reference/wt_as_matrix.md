# Convert neighbor or weights list to matrix

Given a `nb` list or weights list, convert them to a matrix.

## Usage

``` r
wt_as_matrix(nb, wt)

nb_as_matrix(nb)
```

## Arguments

- nb:

  a neighbor list—i.e. as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- wt:

  a weights list—i.e. as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md)

## Value

Returns a n x n matrix

## Examples

``` r

# make a grid
g <- sf::st_make_grid(
  cellsize = c(10, 10),
  offset = c(0, 0),
  n = c(2, 2)
)

# create neighbors
nb <- st_contiguity(g)

# cast to matrix
nb_as_matrix(nb)
#>   1 2 3 4
#> 1 0 1 1 1
#> 2 1 0 1 1
#> 3 1 1 0 1
#> 4 1 1 1 0
#> attr(,"call")
#> spdep::nb2mat(neighbours = nb, style = "B")

# create weights
wt <- st_weights(nb)

# cast as matrix
wt_as_matrix(nb, wt)
#>           [,1]      [,2]      [,3]      [,4]
#> [1,] 0.0000000 0.3333333 0.3333333 0.3333333
#> [2,] 0.3333333 0.0000000 0.3333333 0.3333333
#> [3,] 0.3333333 0.3333333 0.0000000 0.3333333
#> [4,] 0.3333333 0.3333333 0.3333333 0.0000000
```
