# Checks geometry for polygons.

If the provided geometry is a polygon, a point will be generated using
[`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).
If a centroid is preferred, a new column can be created that contains
the output of
[`sf::st_centroid()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

## Usage

``` r
check_polygon(geometry)
```

## Arguments

- geometry:

  an sfc object

## Value

Point geometry
