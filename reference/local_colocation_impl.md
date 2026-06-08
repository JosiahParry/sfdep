# spdep implementation of local colocation quotient

Internal implementation of the local CLQ that is compatible with spdep.

## Usage

``` r
local_colocation_impl(A, B, listw, nsim = 99)
```

## Value

a data frame where the number of rows is the same length as `A` and the
number of columns is the same as unique values in `B`.
