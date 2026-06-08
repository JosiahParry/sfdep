# Create a listw object from a neighbors and weight list

Given a neighbor and weight list, create a `listw` object.

## Usage

``` r
recreate_listw(nb, wt)
```

## Arguments

- nb:

  a neighbor list object for example as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- wt:

  a weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

## Value

a `listw` object

## Examples

``` r
recreate_listw(guerry_nb$nb, guerry_nb$wt)
#> Characteristics of weights list object:
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 420 
#> Percentage nonzero weights: 5.813149 
#> Average number of links: 4.941176 
#> 
#> Weights style: W 
#> Weights constants summary:
#>    n   nn S0      S1       S2
#> W 85 7225 85 37.2761 347.6683
```
