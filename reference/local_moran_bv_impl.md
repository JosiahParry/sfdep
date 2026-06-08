# Local Bivariate Moran's I spdep implementation

Local Bivariate Moran's I spdep implementation

## Usage

``` r
local_moran_bv_impl(x, y, listw, nsim)
```

## Arguments

- x:

  a numeric vector of same length as `y`.

- y:

  a numeric vector of same length as `x`.

- listw:

  a listw object for example as created by `nb2listw()`.

- nsim:

  the number of simulations to run.
