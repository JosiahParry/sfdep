# Create an Ellipse

Generate an ellipse from center coordinates, major and minor axis radii,
and angle rotation.

## Usage

``` r
ellipse(x = 0, y = 0, sx = 2, sy = 1, rotation = 0, n = 100)

st_ellipse(geometry, sx, sy, rotation = 0, n = 100)
```

## Arguments

- x:

  longitude of center point

- y:

  latitude of center point

- sx:

  radius of major axis

- sy:

  radius of minor axis

- rotation:

  the degree of rotation of the ellipse

- n:

  the number of coordinates to generate for the ellipse

- geometry:

  an sf `ST_POINT` geometry. Can be `sfg`, `sfc`, or `sf` object

## Value

an sf object

## Details

`ellipse()` returns a matrix of point locations defining the ellipse.
`st_ellipse()` returns an sf object with LINE geography of the ellipse.
Increasing n increases the number of points generated to define the
ellipse shape.

`ellipse()` function is adapted from `ggVennDiagram`.

## Examples

``` r
ellipse(n = 10)
#>               x             y
#>  [1,]  2.000000  0.000000e+00
#>  [2,]  1.618034  5.877853e-01
#>  [3,]  0.618034  9.510565e-01
#>  [4,] -0.618034  9.510565e-01
#>  [5,] -1.618034  5.877853e-01
#>  [6,] -2.000000  1.224647e-16
#>  [7,] -1.618034 -5.877853e-01
#>  [8,] -0.618034 -9.510565e-01
#>  [9,]  0.618034 -9.510565e-01
#> [10,]  1.618034 -5.877853e-01
#> [11,]  2.000000  0.000000e+00
st_ellipse(sf::st_point(c(0, 0)), sx = 10, sy = 10)
#> Geometry set for 1 feature 
#> Geometry type: LINESTRING
#> Dimension:     XY
#> Bounding box:  xmin: -10 ymin: -10 xmax: 10 ymax: 10
#> CRS:           NA
#> LINESTRING (10 0, 9.980267 0.6279052, 9.921147 ...
```
