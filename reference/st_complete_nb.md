# Create Neighbors as Complete Graph

Create a neighbors list where every element is related to every other
element. This creates a complete graph.

## Usage

``` r
st_complete_nb(n_elements, diag = FALSE)
```

## Arguments

- n_elements:

  the number of observations to create a neighbors list for

- diag:

  default `FALSE`. If `TRUE`, includes diagonal element / the self.

## Value

A neighbors list representing a complete graph.

## Examples

``` r
st_complete_nb(5)
#> Neighbour list object:
#> Number of regions: 5 
#> Number of nonzero links: 20 
#> Percentage nonzero weights: 80 
#> Average number of links: 4 
```
