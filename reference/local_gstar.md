# Local G\*

Calculate the local Gi\* statistic.

## Usage

``` r
local_gstar(x, nb, wt, alternative = "two.sided", ...)

local_gstar_perm(x, nb, wt, nsim = 499, alternative = "two.sided", ...)
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

- `e_gi`: the permutation sample mean

- `var_gi`: the permutation sample variance

- `p_value`: the p-value using sample mean and standard deviation

- `p_folded_sim`: p-value based on the implementation of Pysal which
  always assumes a two-sided test taking the minimum possible p-value

- `skewness`: sample skewness

- `kurtosis`: sample kurtosis

## Examples

``` r
nb <- st_contiguity(guerry)
wt <- st_weights(nb)
x <- guerry$crime_pers

res <- local_gstar_perm(x, nb, wt)
head(res)
#>     gi_star cluster       e_gi       var_gi    std_dev    p_value p_sim
#> 1  1.342359    High 0.01281212 2.828987e-06  0.8669055 0.38599380 0.408
#> 2  2.595578    High 0.01231754 2.534491e-06  2.1922882 0.02835870 0.036
#> 3  2.388872    High 0.01226355 2.355173e-06  2.0995950 0.03576448 0.044
#> 4 -1.848187     Low 0.01098208 2.788616e-06 -1.5970973 0.11024403 0.120
#> 5 -1.209736     Low 0.01146742 3.231416e-06 -1.2477284 0.21213055 0.212
#> 6 -2.034142     Low 0.01097455 1.880792e-06 -1.5710781 0.11616450 0.120
#>   p_folded_sim    skewness    kurtosis
#> 1        0.204  0.11601646 -0.37505144
#> 2        0.018  0.13538275 -0.07345225
#> 3        0.022  0.07291573 -0.24293931
#> 4        0.060 -0.05713113 -0.15455315
#> 5        0.106  0.13602702 -0.40875762
#> 6        0.060  0.05456544 -0.27237752

res <- local_gstar(x, nb, wt)
head(res)
#> [1]  1.342359  2.595578  2.388872 -1.848187 -1.209736 -2.034142
```
