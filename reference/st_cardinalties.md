# Calculate neighbor cardinalities

Identify the cardinality of a neighbor object. Utilizes
[`spdep::card()`](https://r-spatial.github.io/spdep/reference/card.html)
for objects with class `nb`, otherwise returns `lengths(nb)`.

## Usage

``` r
st_cardinalties(nb)
```

## Arguments

- nb:

  A neighbor list object as created by `st_neighbors()`.

## Value

an integer vector with the same length as `nb`.

## See also

Other other:
[`st_nb_lag()`](https://josiahparry.github.io/sfdep/reference/st_nb_lag.md),
[`st_nb_lag_cumul()`](https://josiahparry.github.io/sfdep/reference/st_nb_lag_cumul.md)

## Examples

``` r
nb <- st_contiguity(sf::st_geometry(guerry))
st_cardinalties(nb)
#>  [1] 4 6 6 4 3 7 3 3 5 5 7 3 3 6 5 5 6 6 7 3 6 7 2 5 6 6 2 6 6 6 4 4 6 6 5 6 5 4
#> [39] 6 7 5 4 7 6 6 5 8 4 6 6 5 4 5 4 2 6 3 6 6 2 6 3 3 2 4 2 4 5 7 6 2 3 8 6 5 5
#> [77] 5 6 3 6 4 6 6 6 5
```
