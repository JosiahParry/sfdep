# Compute the Global Bivariate Moran's I

Given two continuous numeric variables, calculate the bivariate Moran's
I. See details for more.

## Usage

``` r
global_moran_bv(x, y, nb, wt, nsim = 99, scale = TRUE)
```

## Arguments

- x:

  a numeric vector of same length as `y`.

- y:

  a numeric vector of same length as `x`.

- nb:

  a neighbor list object for example as created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

- wt:

  a weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

- nsim:

  the number of simulations to run.

- scale:

  default `TRUE`.

## Value

an object of class boot

## Details

The Global Bivariate Moran is defined as

\\ I_B = \frac{\Sigma_i(\Sigma_j{w\_{ij}y_j\times
x_i})}{\Sigma_i{x_i^2}} \\

It is important to note that this is a measure of autocorrelation of X
with the spatial lag of Y. As such, the resultant measure may
overestimate the amount of spatial autocorrelation which may be a
product of the inherent correlation of X and Y.

## References

[Global Spatial Autocorrelation (2): Bivariate, Differential and EB Rate
Moran Scatter Plot, Luc
Anselin](https://geodacenter.github.io/workbook/5b_global_adv/lab5b.html)

## See also

Other global_moran:
[`global_moran()`](https://josiahparry.github.io/sfdep/reference/global_moran.md),
[`global_moran_perm()`](https://josiahparry.github.io/sfdep/reference/global_moran_perm.md),
[`global_moran_test()`](https://josiahparry.github.io/sfdep/reference/global_moran_test.md),
[`local_moran_bv()`](https://josiahparry.github.io/sfdep/reference/local_moran_bv.md)

## Examples

``` r
x <- guerry_nb$crime_pers
y <- guerry_nb$wealth
nb <- guerry_nb$nb
wt <- guerry_nb$wt
global_moran_bv(x, y, nb, wt)
#> 
#> DATA PERMUTATION
#> 
#> 
#> Call:
#> boot(data = xx, statistic = bvm_boot, R = nsim, sim = "permutation", 
#>     listw = listw, parallel = parallel, ncpus = ncpus, cl = cl)
#> 
#> 
#> Bootstrap Statistics :
#>       original    bias    std. error
#> t1* -0.1006674 0.1024929  0.05090268
```
