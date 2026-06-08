# Global C Test

Global C Test

## Usage

``` r
global_c_test(x, nb, wt, randomization = TRUE, allow_zero = NULL, ...)
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

- randomization:

  default `TRUE`. Calculate variance based on randomization. If `FALSE`,
  under the assumption of normality.

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  additional arguments passed to
  [`spdep::moran.mc()`](https://r-spatial.github.io/spdep/reference/moran.mc.html)

## Value

an `htest` object

## See also

Other global_c:
[`global_c()`](https://josiahparry.github.io/sfdep/reference/global_c.md),
[`global_c_perm()`](https://josiahparry.github.io/sfdep/reference/global_c_perm.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wt <- st_weights(nb)
x <- guerry$crime_pers
global_c_test(x, nb, wt)
#> 
#>  Geary C test under randomisation
#> 
#> data:  x 
#> weights: listw   
#> 
#> Geary C statistic standard deviate = 6.0556, p-value = 6.994e-10
#> alternative hypothesis: Expectation greater than statistic
#> sample estimates:
#> Geary C statistic       Expectation          Variance 
#>       0.564590693       1.000000000       0.005169871 
#> 
```
