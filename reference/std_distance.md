# Calculate standard distance

The standard distance of a point pattern is a measure of central
tendency. Standard distance measures distance away from the mean center
of the point pattern similar to standard deviations.

## Usage

``` r
std_distance(geometry)
```

## Arguments

- geometry:

  an sfc object. If a polygon, uses
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html).

## Value

A numeric scalar.

## See also

Other point-pattern:
[`center_mean()`](https://josiahparry.github.io/sfdep/reference/center_mean.md)

## Examples

``` r
# Make a grid to sample from
grd <- sf::st_make_grid(n = c(1, 1), cellsize = c(100, 100), offset = c(0,0))

# sample 100 points
pnts <- sf::st_sample(grd, 100)

# plot points
plot(pnts)


# calculate standard distance
std_distance(pnts)
#> [1] 37.06036
```
