# Getis-Ord Global G

Getis-Ord Global G

## Usage

``` r
global_g_test(x, nb, wt, alternative = "greater", allow_zero = NULL, ...)
```

## Arguments

- x:

  A numeric vector.

- nb:

  a neighbor list object for example as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- wt:

  a weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

- alternative:

  default `"two.sided"`. Should be one of `"greater"`, `"less"`, or
  `"two.sided"` to specify the alternative hypothesis.

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  additional methods passed to
  [`spdep::globalG.test()`](https://r-spatial.github.io/spdep/reference/globalG.test.html).

## Value

an `htest` object

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wt <- st_weights(nb, style = "B")
x <- guerry$crime_pers
global_g_test(x, nb, wt)
#> 
#>  Getis-Ord global G statistic
#> 
#> data:  x 
#> weights: listw   
#> 
#> standard deviate = 3.8277, p-value = 6.467e-05
#> alternative hypothesis: greater
#> sample estimates:
#> Global G statistic        Expectation           Variance 
#>       6.464853e-02       5.882353e-02       2.315853e-06 
#> 
```
