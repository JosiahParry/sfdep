# Calculate spatial lag

Calculates the spatial lag of a numeric variable given a neighbor and
weights list.

## Usage

``` r
st_lag(x, nb, wt, na_ok = FALSE, allow_zero = NULL, ...)
```

## Arguments

- x:

  A numeric vector

- nb:

  A neighbor list object as created by `st_neighbors()`.

- wt:

  A weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

- na_ok:

  Default `FALSE`. If, `TRUE` missing values return a lagged `NA`.

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  See
  [`?spdep::lag.listw`](https://r-spatial.github.io/spdep/reference/lag.listw.html)
  for more.

## Value

a numeric vector with same length as `x`

## See also

Other stats:
[`local_moran()`](https://josiahparry.github.io/sfdep/reference/local_moran.md)

## Examples

``` r
geo <- sf::st_geometry(guerry)
nb <- st_contiguity(geo)
wt <- st_weights(nb)

st_lag(guerry$crime_pers, nb, wt)
#>  [1] 23047.50 26919.67 26195.50 14401.25 15038.67 15749.00 22111.67 13672.33
#>  [9] 22859.20 11475.20 12200.14 13278.67 24734.00 11752.83 17992.60 21974.40
#> [17] 26711.00 19344.00 23696.71 25108.67 21643.17 18306.00 24280.00 14451.60
#> [25] 21047.67 21421.33 25961.50 10869.83 13415.67 17172.17 20238.25 12504.25
#> [33] 26723.00 21772.83 26462.20 19252.00 24683.20 20607.25 24412.00 19373.71
#> [41] 16000.20 23993.25 20337.86 16818.67 17113.83 13013.00 22133.00 24093.75
#> [49] 25661.67 22190.17 29030.00 16951.00 24509.00 24982.75 19491.50 24176.00
#> [57] 27639.67 21274.33 24510.33 30166.00 23459.00 16184.00 18002.00 10910.00
#> [65] 16251.25 15572.00 25884.25 23020.60 26495.00 24690.50 17339.00 25522.33
#> [73] 18970.00 19701.83 21841.00 24520.40 14025.80 14565.17 13306.67 12579.00
#> [81] 21529.50 23474.50 24373.17 19900.50 23373.60
```
