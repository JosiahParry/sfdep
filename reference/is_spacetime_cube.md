# Test if a spacetime object is a spacetime cube

Given an object with class `spacetime`, determine if it is a *spacetime
cube*. If the time-series is is irregular a warning is emitted (see
[`validate_spacetime()`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
for more on the restrictions on the time column.

## Usage

``` r
is_spacetime_cube(x, ...)
```

## Arguments

- x:

  a spacetime object

- ...:

  unused

## Value

A logical scalar.

## Details

A spacetime object is a spacetime cube when it contains a regular
time-series representation of each geometry. That is, only one
observation for at each time period per geography is present.

The number of rows in a spacetime cube is the number of geographies
multiplied by the number of time periods. For example if there are 10
locations and 20 time periods, the number of rows must be 200.

## Validation

`is_spacetime_cube()` runs a number of checks that to ensure that the
provided object is in fact a spacetime cube. It checks that:

- the number of rows is equal to the number of locations multiplied by
  the number of time periods

- each time period has an equal number of observations

- each location has an equal number of observations

- each combination of time period and location has only one observation

- that the time-series is regular

## Examples

``` r

if (requireNamespace("zoo", quietly = TRUE)) {
df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")

# read in data
df <- read.csv(
  df_fp, colClasses = c("character", "character", "integer", "double", "Date")
)
geo <- sf::st_read(geo_fp)

# Create spacetime object called `bos`
bos <- spacetime(df, geo,
                .loc_col = ".region_id",
                .time_col = "time_period")

is_spacetime_cube(bos)
is_spacetime_cube(bos[round(runif(1000, 0, nrow(bos))),])
is_spacetime_cube(guerry)
}
#> Reading layer `bos-ecometric' from data source 
#>   `/home/runner/work/_temp/Library/sfdep/extdata/bos-ecometric.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 168 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.19115 ymin: 42.22788 xmax: -70.99445 ymax: 42.3974
#> Geodetic CRS:  NAD83
#> ! Number of rows does not equal `n time-periods x n locations`
#> [1] FALSE
```
