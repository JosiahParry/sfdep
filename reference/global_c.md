# Compute Geary's C

Compute Geary's C

## Usage

``` r
global_c(x, nb, wt, allow_zero = NULL)
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

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

## Value

a list with two names elements `C` and `K` returning the value of
Geary's C and sample kurtosis respectively.

## See also

Other global_c:
[`global_c_perm()`](https://josiahparry.github.io/sfdep/reference/global_c_perm.md),
[`global_c_test()`](https://josiahparry.github.io/sfdep/reference/global_c_test.md)

## Examples

``` r
nb <- guerry_nb$nb
wt <- guerry_nb$wt
x <- guerry_nb$crime_pers
global_c(x, nb, wt)
#> $C
#> [1] 0.5645907
#> 
#> $K
#> [1] 2.400641
#> 
```
