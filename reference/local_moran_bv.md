# Compute the Local Bivariate Moran's I Statistic

Given two continuous numeric variables, calculate the bivariate Local
Moran's I.

## Usage

``` r
local_moran_bv(x, y, nb, wt, nsim = 499)
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

## Value

a `data.frame` containing two columns `Ib` and `p_sim` containing the
local bivariate Moran's I and simulated p-values respectively.

## Details

The Bivariate Local Moran, like its global counterpart, evaluates the
value of x at observation i with its spatial neighbors' value of y. The
value of \$\$I_i^B\$\$ is just xi \* Wyi. Or, in simpler words the local
bivariate Moran is the result of multiplying x by the spatial lag of y.
Formally it is defined as

\\ I_i^B= cx_i\Sigma_j{w\_{ij}y_j} \\

## References

[Local Spatial Autocorrelation (3): Multivariate Local Spatial
Autocorrelation, Luc
Anselin](https://geodacenter.github.io/workbook/6c_local_multi/lab6c.html#principle)

## See also

Other global_moran:
[`global_moran()`](https://josiahparry.github.io/sfdep/reference/global_moran.md),
[`global_moran_bv()`](https://josiahparry.github.io/sfdep/reference/global_moran_bv.md),
[`global_moran_perm()`](https://josiahparry.github.io/sfdep/reference/global_moran_perm.md),
[`global_moran_test()`](https://josiahparry.github.io/sfdep/reference/global_moran_test.md)

## Examples

``` r
x <- guerry_nb$crime_pers
y <- guerry_nb$wealth
nb <- guerry_nb$nb
wt <- guerry_nb$wt
local_moran_bv(x, y, nb, wt)
#>              Ib p_sim
#> 1   0.093506129 0.430
#> 2  -0.840144689 0.002
#> 3   0.293390275 0.230
#> 4  -0.782969282 0.036
#> 5  -0.343051282 0.026
#> 6  -0.751763780 0.060
#> 7  -1.212286959 0.168
#> 8   0.494768137 0.354
#> 9   0.050875987 0.012
#> 10 -0.033508122 0.398
#> 11  0.119135965 0.398
#> 12 -0.027058053 0.476
#> 13  0.189608296 0.124
#> 14 -0.184931534 0.034
#> 15  0.186274796 0.260
#> 16 -0.042411941 0.298
#> 17  0.006353979 0.486
#> 18 -0.408271216 0.050
#> 19 -0.115344980 0.444
#> 20  0.130059701 0.446
#> 21  1.683274652 0.036
#> 22 -0.003842500 0.484
#> 23 -1.004957821 0.122
#> 24 -1.104136137 0.000
#> 25  0.857180921 0.002
#> 26 -0.169492824 0.004
#> 27  0.834090623 0.186
#> 28 -0.171464343 0.330
#> 29 -0.034233828 0.348
#> 30 -0.051026184 0.232
#> 31 -0.046850788 0.436
#> 32  0.334455413 0.270
#> 33  0.080264717 0.244
#> 34  0.685071940 0.178
#> 35  0.001704856 0.492
#> 36 -0.079714992 0.102
#> 37  0.137434262 0.352
#> 38  0.125025064 0.192
#> 39 -0.068341930 0.194
#> 40  0.504579881 0.096
#> 41 -0.319036396 0.070
#> 42 -0.016497540 0.350
#> 43  0.225196989 0.026
#> 44 -0.224572220 0.364
#> 45 -0.003990683 0.438
#> 46 -1.017640129 0.088
#> 47  0.367618246 0.202
#> 48 -0.550574797 0.244
#> 49  0.441828404 0.034
#> 50 -0.088146734 0.400
#> 51 -0.190742450 0.338
#> 52  0.537482661 0.114
#> 53 -0.039104825 0.414
#> 54  0.305236945 0.076
#> 55 -0.337570608 0.330
#> 56 -0.155974076 0.266
#> 57 -0.526846963 0.182
#> 58 -1.393680416 0.000
#> 59 -0.832978265 0.024
#> 60 -0.326847547 0.152
#> 61 -0.269459435 0.038
#> 62 -0.443421284 0.030
#> 63 -0.158620813 0.392
#> 64 -0.066683215 0.488
#> 65 -0.790102037 0.074
#> 66 -1.406140486 0.156
#> 67 -0.058461082 0.226
#> 68 -0.004884983 0.488
#> 69 -0.125083947 0.340
#> 70 -1.236566942 0.042
#> 71  1.282709560 0.006
#> 72  0.218277696 0.042
#> 73 -0.366089264 0.000
#> 74  1.371117985 0.002
#> 75 -0.075151611 0.208
#> 76 -1.560168225 0.022
#> 77  0.658190968 0.078
#> 78  0.246687028 0.238
#> 79 -0.139720645 0.420
#> 80 -0.206370145 0.296
#> 81 -0.003905568 0.458
#> 82 -0.205045471 0.176
#> 83 -0.534172545 0.002
#> 84 -0.046631364 0.220
#> 85  0.279224938 0.008
```
