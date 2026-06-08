# Apply a function to neighbors

Sometimes one may want to create custom lag variables or create some
other neighborhood level metric that may not be defined yet. This
`st_nb_apply()` enables you to apply a function to each observation's
(xi) neighbors (xij).

## Usage

``` r
st_nb_apply(x, nb, wt, .f, suffix = "dbl", ...)
```

## Arguments

- x:

  A vector that will be used for neighbor xij values.

- nb:

  A neighbor list object as created by `st_neighbors()`.

- wt:

  A weights list as created by
  [`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md).

- .f:

  A function definition. There are three default objects that can be
  used inside of the function definition:

  - `.xij`: neighbor values of `x` for the ith observation. This is
    simply the subset of x based on the corresponding `nb` list values
    for each element.

  - `.nb`: neighbor positions.

  - `.wt`: neighbor weights value.

  If any of these three function arguments are omitted from `.f`, dots
  (`...`) must be supplied.

- suffix:

  The `map` variant to use. Options are "dbl", "int", "lgl", "chr",
  "list".

- ...:

  arguments to pass to `.f`

## Value

a vector or list of with same length as `x`.

## Details

The below example calculates the spatial lag using `st_nb_apply()` and
[`st_lag()`](https://josiahparry.github.io/sfdep/reference/st_lag.md) to
illustrate how we can apply functions to neighbors.

Currently questioning the use case.
[`find_xj()`](https://josiahparry.github.io/sfdep/reference/find_xj.md)
is now exported and may negate the need for this function.

## Examples

``` r
if (requireNamespace("dplyr", quietly = TRUE)) {
library(magrittr)
guerry %>%
  dplyr::transmute(
    nb = st_contiguity(geometry),
    wt = st_weights(nb),
    lag_apply = st_nb_apply(
      crime_pers, nb, wt,
      .f = function(.xij, .wt, ...) sum(.xij *.wt)
    ),
    lag = st_lag(crime_pers, nb, wt)
  )
}
#> Simple feature collection with 85 features and 4 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 85 × 5
#>    nb        wt        lag_apply    lag                                 geometry
#>  * <nb>      <list>        <dbl>  <dbl>                           <MULTIPOLYGON>
#>  1 <int [4]> <dbl [4]>    23048. 23048. (((801150 2092615, 800669 2093190, 8006…
#>  2 <int [6]> <dbl [6]>    26920. 26920. (((729326 2521619, 729320 2521230, 7292…
#>  3 <int [6]> <dbl [6]>    26196. 26195. (((710830 2137350, 711746 2136617, 7124…
#>  4 <int [4]> <dbl [4]>    14401. 14401. (((882701 1920024, 882408 1920733, 8817…
#>  5 <int [3]> <dbl [3]>    15039. 15039. (((886504 1922890, 885733 1922978, 8854…
#>  6 <int [7]> <dbl [7]>    15749  15749  (((747008 1925789, 746630 1925762, 7457…
#>  7 <int [3]> <dbl [3]>    22112. 22112. (((818893 2514767, 818614 2514515, 8179…
#>  8 <int [3]> <dbl [3]>    13672. 13672. (((509103 1747787, 508820 1747513, 5081…
#>  9 <int [5]> <dbl [5]>    22859. 22859. (((775400 2345600, 775068 2345397, 7735…
#> 10 <int [5]> <dbl [5]>    11475. 11475. (((626230 1810121, 626269 1810496, 6274…
#> # ℹ 75 more rows
```
