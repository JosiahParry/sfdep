# Global sum of weights

Calculate the global sum of weights

## Usage

``` r
szero(wt)
```

## Arguments

- wt:

  a weights list—i.e. created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md)

## Value

a scalar numeric

## Examples

``` r
nb <- st_contiguity(guerry)
wt <- st_weights(nb)
szero(wt)
#> [1] 85
```
