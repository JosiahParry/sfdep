# Local G

Calculate the local Geary statistic for a given variable.

## Usage

``` r
local_g(x, nb, wt, alternative = "two.sided", ...)

local_g_perm(x, nb, wt, nsim = 499, alternative = "two.sided", ...)
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

- ...:

  methods passed to
  [`spdep::localG()`](https://r-spatial.github.io/spdep/reference/localG.html)
  or
  [`spdep::localG_perm()`](https://r-spatial.github.io/spdep/reference/localG.html)

- nsim:

  The number of simulations to run.

## Value

a `data.frame` with columns:

- `gi`: the observed statistic

- `cluster`: factor variable with two levels classification high or low

- `e_gi`: the permutation sample mean

- `var_gi`: the permutation sample variance

- `std_dev`: standard deviation of the Gi statistic

- `p_value`: the p-value using sample mean and standard deviation

- `p_folded_sim`: p-value based on the implementation of Pysal which
  always assumes a two-sided test taking the minimum possible p-value

- `skewness`: sample skewness

- `kurtosis`: sample kurtosis

## Examples

``` r
x <- guerry$crime_pers
nb <- st_contiguity(guerry)
wt <- st_weights(nb)

res <- local_g_perm(x, nb, wt)

head(res)
#>           gi cluster       e_gi       var_gi    std_dev    p_value p_sim
#> 1  0.8991367    High 0.01181669 5.294820e-06  0.8701837 0.38420001 0.424
#> 2  2.4455980    High 0.01191097 2.889082e-06  2.4734589 0.01338122 0.008
#> 3  2.1976551    High 0.01190228 2.679746e-06  2.3117279 0.02079268 0.036
#> 4 -1.5838690     Low 0.01188818 4.847045e-06 -1.5148402 0.12981289 0.116
#> 5 -1.1902425     Low 0.01187490 6.603139e-06 -1.1359518 0.25597672 0.252
#> 6 -1.6527255     Low 0.01185289 2.372027e-06 -1.6352541 0.10199579 0.104
#>   p_folded_sim    skewness    kurtosis
#> 1        0.212  0.06130549 -0.23895408
#> 2        0.004 -0.13947731 -0.09884521
#> 3        0.018  0.21507002  0.01754043
#> 4        0.058  0.09450422 -0.30518697
#> 5        0.126  0.22713868  0.04683920
#> 6        0.052  0.16068469  0.07086942
```
