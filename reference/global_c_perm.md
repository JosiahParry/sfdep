# Global C Permutation Test

Global C Permutation Test

## Usage

``` r
global_c_perm(
  x,
  nb,
  wt,
  nsim = 499,
  alternative = "greater",
  allow_zero = NULL,
  ...
)
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

- nsim:

  number of simulations to run.

- alternative:

  default `"two.sided"`. Should be one of `"greater"`, `"less"`, or
  `"two.sided"` to specify the alternative hypothesis.

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  additional arguments passed to
  [`spdep::geary.mc()`](https://r-spatial.github.io/spdep/reference/geary.mc.html).

## Value

an object of classes `htest` and `mc.sim`

## See also

Other global_c:
[`global_c()`](https://josiahparry.github.io/sfdep/reference/global_c.md),
[`global_c_test()`](https://josiahparry.github.io/sfdep/reference/global_c_test.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wt <- st_weights(nb)
x <- guerry$crime_pers
global_c_perm(x, nb, wt)
#> 
#>  Monte-Carlo simulation of Geary C
#> 
#> data:  x 
#> weights: listw  
#> number of simulations + 1: 500 
#> 
#> statistic = 0.56459, observed rank = 1, p-value = 0.002
#> alternative hypothesis: greater
#> 
```
