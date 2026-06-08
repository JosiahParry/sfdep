# Calculation Standard Deviational Ellipse

From an sf object containing points, calculate the standard deviational
ellipse.

## Usage

``` r
std_dev_ellipse(geometry)
```

## Arguments

- geometry:

  an sfc object. If a polygon, uses
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

## Value

An sf object with three columns

- `sx`: major axis radius in CRS units,

- `sy`: minor axis radius in CRS units,

- `theta`: degree rotation of the ellipse.

sf object's geometry is the center mean point.

## Details

The bulk of this function is derived from the archived CRAN package
aspace version 3.2.0.

## Examples

``` r
#' # Make a grid to sample from
grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))

# sample 100 points
pnts <- sf::st_sample(grd, 100)
std_dev_ellipse(pnts)
#> Simple feature collection with 1 feature and 3 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: 52.84892 ymin: 51.07882 xmax: 52.84892 ymax: 51.07882
#> CRS:           NA
#>        sx       sy    theta                  geometry
#> 1 39.8631 42.98488 133.0087 POINT (52.84892 51.07882)
```
