# Global Moran Test

Global Moran Test

## Usage

``` r
global_moran_test(
  x,
  nb,
  wt,
  alternative = "greater",
  randomization = TRUE,
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

- alternative:

  default `"two.sided"`. Should be one of `"greater"`, `"less"`, or
  `"two.sided"` to specify the alternative hypothesis.

- randomization:

  default `TRUE`. Calculate variance based on randomization. If `FALSE`,
  under the assumption of normality.

- ...:

  additional arguments passed to
  [`spdep::moran.mc()`](https://r-spatial.github.io/spdep/reference/moran.mc.html)

## Value

an object of class `htest`

## See also

Other global_moran:
[`global_moran()`](https://josiahparry.github.io/sfdep/reference/global_moran.md),
[`global_moran_bv()`](https://josiahparry.github.io/sfdep/reference/global_moran_bv.md),
[`global_moran_perm()`](https://josiahparry.github.io/sfdep/reference/global_moran_perm.md),
[`local_moran_bv()`](https://josiahparry.github.io/sfdep/reference/local_moran_bv.md)

## Examples

``` r
nb <- guerry_nb$nb
wt <- guerry_nb$wt
x <- guerry_nb$crime_pers
global_moran_test(x, nb, wt)
#> 
#>  Moran I test under randomisation
#> 
#> data:  x  
#> weights: listw    
#> 
#> Moran I statistic standard deviate = 6.0484, p-value = 7.316e-10
#> alternative hypothesis: greater
#> sample estimates:
#> Moran I statistic       Expectation          Variance 
#>       0.411459718      -0.011904762       0.004899501 
#> 
```
