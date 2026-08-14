# Create Block Contiguity for Spatial Regimes

libpysal write that "block contiguity structures are relevant when
defining neighbor relations based on membership in a regime. For
example, all counties belonging to the same state could be defined as
neighbors, in an analysis of all counties in the US."

Source:
[libpysal](https://pysal.org/libpysal/generated/libpysal.weights.block_weights.html)

## Usage

``` r
st_block_nb(regime, id = seq_along(regime), diag = FALSE)
```

## Arguments

- regime:

  a column identifying which spatial regime each element of `id` belongs

- id:

  a column identifying unique observations

- diag:

  default `FALSE`. If `TRUE`, includes diagonal element / the self.

## Value

An object of class `nb`. When `diag = TRUE` the attribute
`self.included = TRUE`.

## Examples

``` r
id <- guerry$code_dept
regime <- guerry$region
st_block_nb(regime, id)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 1360 
#> Percentage nonzero weights: 18.82353 
#> Average number of links: 16 
#> 5 disjoint connected subgraphs
```
