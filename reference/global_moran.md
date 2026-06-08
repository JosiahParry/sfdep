# Calculate Global Moran's I

Calculate Global Moran's I

## Usage

``` r
global_moran(x, nb, wt, na_ok = FALSE, ...)
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

- na_ok:

  default `FALSE`. If `FALSE` presence or `NA` or `Inf` results in an
  error.

- ...:

  additional arguments passed to
  [`spdep::moran()`](https://r-spatial.github.io/spdep/reference/moran.html).

## Value

an `htest` object

## See also

Other global_moran:
[`global_moran_bv()`](https://josiahparry.github.io/sfdep/reference/global_moran_bv.md),
[`global_moran_perm()`](https://josiahparry.github.io/sfdep/reference/global_moran_perm.md),
[`global_moran_test()`](https://josiahparry.github.io/sfdep/reference/global_moran_test.md),
[`local_moran_bv()`](https://josiahparry.github.io/sfdep/reference/local_moran_bv.md)

## Examples

``` r
nb <- guerry_nb$nb
wt <- guerry_nb$wt
x <- guerry_nb$crime_pers
moran <- global_moran(x, nb, wt)
```
