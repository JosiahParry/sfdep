# Global Join Counts

Calculate global join count measure for a categorical variable.

## Usage

``` r
global_jc_perm(
  fx,
  nb,
  wt,
  alternative = "greater",
  nsim = 499,
  allow_zero = FALSE,
  ...
)

global_jc_test(fx, nb, wt, alternative = "greater", allow_zero = NULL, ...)

tally_jc(fx, nb, wt, allow_zero = TRUE, ...)
```

## Arguments

- fx:

  a factor or character vector of the same length as nb.

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

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  additional arguments passed to methods

## Value

an object of class `jclist` which is a list where each element is of
class `htest` and `mc.sim`.

## Details

- `global_jc_perm()` implements the monte-carlo based join count using
  [`spdep::joincount.mc()`](https://r-spatial.github.io/spdep/reference/joincount.mc.html)

- `global_jc_test()` implements the traditional BB join count statistic
  using
  [`spdep::joincount.test()`](https://r-spatial.github.io/spdep/reference/joincount.test.html)

- `tally_jc()` calculated join counts for a variable `fx` and returns a
  data.frame using
  [`spdep::joincount.multi()`](https://r-spatial.github.io/spdep/reference/joincount.multi.html)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wt <- st_weights(nb, style = "B")
fx <- guerry$region
global_jc_perm(fx, nb, wt)
#> 
#>  Monte-Carlo simulation of join-count statistic
#> 
#> data:  fx 
#> weights: listw 
#> number of simulations + 1: 500 
#> 
#> Join-count statistic for C = 35, rank of observed statistic = 500,
#> p-value = 0.002
#> alternative hypothesis: greater
#> sample estimates:
#>     mean of simulation variance of simulation 
#>               7.893788               5.781869 
#> 
#> 
#>  Monte-Carlo simulation of join-count statistic
#> 
#> data:  fx 
#> weights: listw 
#> number of simulations + 1: 500 
#> 
#> Join-count statistic for E = 29, rank of observed statistic = 500,
#> p-value = 0.002
#> alternative hypothesis: greater
#> sample estimates:
#>     mean of simulation variance of simulation 
#>               7.875752               5.779712 
#> 
#> 
#>  Monte-Carlo simulation of join-count statistic
#> 
#> data:  fx 
#> weights: listw 
#> number of simulations + 1: 500 
#> 
#> Join-count statistic for N = 29, rank of observed statistic = 500,
#> p-value = 0.002
#> alternative hypothesis: greater
#> sample estimates:
#>     mean of simulation variance of simulation 
#>               8.078156               6.305124 
#> 
#> 
#>  Monte-Carlo simulation of join-count statistic
#> 
#> data:  fx 
#> weights: listw 
#> number of simulations + 1: 500 
#> 
#> Join-count statistic for S = 32, rank of observed statistic = 500,
#> p-value = 0.002
#> alternative hypothesis: greater
#> sample estimates:
#>     mean of simulation variance of simulation 
#>               7.831663               6.296907 
#> 
#> 
#>  Monte-Carlo simulation of join-count statistic
#> 
#> data:  fx 
#> weights: listw 
#> number of simulations + 1: 500 
#> 
#> Join-count statistic for W = 31, rank of observed statistic = 500,
#> p-value = 0.002
#> alternative hypothesis: greater
#> sample estimates:
#>     mean of simulation variance of simulation 
#>               8.098196               6.570659 
#> 

global_jc_test(fx, nb, wt)
#> 
#>  Join count test under nonfree sampling
#> 
#> data:  fx 
#> weights: listw 
#> 
#> Std. deviate for C = 10.886, p-value < 2.2e-16
#> alternative hypothesis: greater
#> sample estimates:
#> Same colour statistic           Expectation              Variance 
#>             35.000000              8.000000              6.151883 
#> 
#> 
#>  Join count test under nonfree sampling
#> 
#> data:  fx 
#> weights: listw 
#> 
#> Std. deviate for E = 8.4667, p-value < 2.2e-16
#> alternative hypothesis: greater
#> sample estimates:
#> Same colour statistic           Expectation              Variance 
#>             29.000000              8.000000              6.151883 
#> 
#> 
#>  Join count test under nonfree sampling
#> 
#> data:  fx 
#> weights: listw 
#> 
#> Std. deviate for N = 8.4667, p-value < 2.2e-16
#> alternative hypothesis: greater
#> sample estimates:
#> Same colour statistic           Expectation              Variance 
#>             29.000000              8.000000              6.151883 
#> 
#> 
#>  Join count test under nonfree sampling
#> 
#> data:  fx 
#> weights: listw 
#> 
#> Std. deviate for S = 9.6763, p-value < 2.2e-16
#> alternative hypothesis: greater
#> sample estimates:
#> Same colour statistic           Expectation              Variance 
#>             32.000000              8.000000              6.151883 
#> 
#> 
#>  Join count test under nonfree sampling
#> 
#> data:  fx 
#> weights: listw 
#> 
#> Std. deviate for W = 9.2731, p-value < 2.2e-16
#> alternative hypothesis: greater
#> sample estimates:
#> Same colour statistic           Expectation              Variance 
#>             31.000000              8.000000              6.151883 
#> 

tally_jc(fx, nb, wt)
#>      joincount expected  variance    z-value joins
#> C:C         35        8  6.151883  10.885785   C:C
#> E:E         29        8  6.151883   8.466721   E:E
#> N:N         29        8  6.151883   8.466721   N:N
#> S:S         32        8  6.151883   9.676253   S:S
#> W:W         31        8  6.151883   9.273076   W:W
#> E:C          8       17 12.887676  -2.507005   E:C
#> N:C          7       17 12.887676  -2.785561   N:C
#> N:E          8       17 12.887676  -2.507005   N:E
#> S:C          7       17 12.887676  -2.785561   S:C
#> S:E          5       17 12.887676  -3.342673   S:E
#> S:N          0       17 12.887676  -4.735454   S:N
#> W:C          9       17 12.887676  -2.228449   W:C
#> W:E          0       17 12.887676  -4.735454   W:E
#> W:N          3       17 12.887676  -3.899786   W:N
#> W:S          7       17 12.887676  -2.785561   W:S
#> Jtot        54      170 30.848971 -20.885152  Jtot
```
