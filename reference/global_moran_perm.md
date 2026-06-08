# Global Moran Permutation Test

Global Moran Permutation Test

## Usage

``` r
global_moran_perm(x, nb, wt, alternative = "two.sided", nsim = 499, ...)
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

- nsim:

  number of simulations to run.

- ...:

  additional arguments passed to
  [`spdep::moran.mc()`](https://r-spatial.github.io/spdep/reference/moran.mc.html)

## Value

an object of classes `htest`, and `mc.sim`.

## See also

Other global_moran:
[`global_moran()`](https://josiahparry.github.io/sfdep/reference/global_moran.md),
[`global_moran_bv()`](https://josiahparry.github.io/sfdep/reference/global_moran_bv.md),
[`global_moran_test()`](https://josiahparry.github.io/sfdep/reference/global_moran_test.md),
[`local_moran_bv()`](https://josiahparry.github.io/sfdep/reference/local_moran_bv.md)

## Examples

``` r
nb <- guerry_nb$nb
wt <- guerry_nb$wt
x <- guerry_nb$crime_pers
moran <- global_moran_perm(x, nb, wt)
moran
#> 
#>  Monte-Carlo simulation of Moran I
#> 
#> data:  x 
#> weights: listw  
#> number of simulations + 1: 500 
#> 
#> statistic = 0.41146, observed rank = 500, p-value < 2.2e-16
#> alternative hypothesis: two.sided
#> 
```
