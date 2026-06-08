# Percent Non-zero Neighbors

Calculate the percentage of non-zero neighbors in a neighbor list.

## Usage

``` r
pct_nonzero(nb)
```

## Arguments

- nb:

  a neighbor list object

## Value

a scalar double

## Examples

``` r
nb <- st_contiguity(guerry)
pct_nonzero(nb)
#> [1] 5.813149
```
