# Pure Higher Order Neighbors

Identify higher order neighbors from a neighbor list. `order` must be
greater than 1. When order equals 2 then the neighbors of the neighbors
list is returned and so forth. See [Anselin's
book](https://spatialanalysis.github.io/handsonspatialdata/index.html)
was: "https://spatial.uchicago.edu"
"/sites/spatial.uchicago.edu/files/1_introandreview_reducedsize.pdf" for
an example.

## Usage

``` r
st_nb_lag(nb, order)
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
[`spdep::nblag()`](https://r-spatial.github.io/spdep/reference/nblag.html)

## See also

Other other:
[`st_cardinalties()`](https://josiahparry.github.io/sfdep/reference/st_cardinalties.md),
[`st_nb_lag_cumul()`](https://josiahparry.github.io/sfdep/reference/st_nb_lag_cumul.md)

## Examples

``` r
nb <- st_contiguity(sf::st_geometry(guerry))
st_nb_lag(nb, 3)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 988 
#> Percentage nonzero weights: 13.67474 
#> Average number of links: 11.62353 
```
