# Calculate spatial weights

Calculate polygon spatial weights from a `nb` list. See
[`spdep::nb2listw()`](https://r-spatial.github.io/spdep/reference/nb2listw.html)
for further details.

## Usage

``` r
st_weights(nb, style = "W", allow_zero = NULL, ...)
```

## Arguments

- nb:

  A neighbor list object as created by `st_neighbors()`.

- style:

  Default `"W"` for row standardized weights. This value can also be
  "B", "C", "U", "minmax", and "S". See
  [`spdep::nb2listw()`](https://r-spatial.github.io/spdep/reference/nb2listw.html)
  for details.

- allow_zero:

  If `TRUE`, assigns zero as lagged value to zone without neighbors.

- ...:

  additional arguments passed to
  [`spdep::nb2listw()`](https://r-spatial.github.io/spdep/reference/nb2listw.html).

## Value

a list where each element is a numeric vector

## Details

Under the hood, `st_weights()` creates a `listw` object and then
extracts the weights elements from it as the `neighbours` element is
already–presumably–already existent in the neighbors list you've already
created. `listw` objects are recreated using
[`recreate_listw()`](https://josiahparry.github.io/sfdep/reference/recreate_listw.md)
when calculating other statistics.

## See also

Other weights:
[`st_inverse_distance()`](https://josiahparry.github.io/sfdep/reference/st_inverse_distance.md),
[`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md),
[`st_nb_dists()`](https://josiahparry.github.io/sfdep/reference/st_nb_dists.md)

## Examples

``` r

if (requireNamespace("dplyr", quietly = TRUE)) {
library(magrittr)
guerry %>%
 dplyr::mutate(nb = st_contiguity(geometry),
               wt = st_weights(nb),
               .before = 1)

}
#> Simple feature collection with 85 features and 28 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 85 × 29
#>    nb        wt    code_dept count ave_id_geo  dept region department crime_pers
#>  * <nb>      <lis> <fct>     <dbl>      <dbl> <int> <fct>  <fct>           <int>
#>  1 <int [4]> <dbl> 01            1         49     1 E      Ain             28870
#>  2 <int [6]> <dbl> 02            1        812     2 N      Aisne           26226
#>  3 <int [6]> <dbl> 03            1       1418     3 C      Allier          26747
#>  4 <int [4]> <dbl> 04            1       1603     4 E      Basses-Al…      12935
#>  5 <int [3]> <dbl> 05            1       1802     5 E      Hautes-Al…      17488
#>  6 <int [7]> <dbl> 07            1       2249     7 S      Ardeche          9474
#>  7 <int [3]> <dbl> 08            1      35395     8 N      Ardennes        35203
#>  8 <int [3]> <dbl> 09            1       2526     9 S      Ariege           6173
#>  9 <int [5]> <dbl> 10            1      34410    10 E      Aube            19602
#> 10 <int [5]> <dbl> 11            1       2807    11 S      Aude            15647
#> # ℹ 75 more rows
#> # ℹ 20 more variables: crime_prop <int>, literacy <int>, donations <int>,
#> #   infants <int>, suicides <int>, main_city <ord>, wealth <int>,
#> #   commerce <int>, clergy <int>, crime_parents <int>, infanticide <int>,
#> #   donation_clergy <int>, lottery <int>, desertion <int>, instruction <int>,
#> #   prostitutes <int>, distance <dbl>, area <int>, pop1831 <dbl>,
#> #   geometry <MULTIPOLYGON>
# using geometry column directly
nb <- st_contiguity(guerry$geometry)
wt <- st_weights(nb)
wt[1:3]
#> [[1]]
#> [1] 0.25 0.25 0.25 0.25
#> 
#> [[2]]
#> [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
#> 
#> [[3]]
#> [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
#> 
```
