# Activate spacetime context

From a
[`spacetime`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
object, activate either the data or geometry contexts. The active object
will then become available for manipulation.

## Usage

``` r
active(.data)

activate(.data, what)
```

## Arguments

- .data:

  a spacetime object

- what:

  default NULL. Determines which context to activate. Valid argument
  values are `"geometry"` and `"data"`. If left null, returns `.data`.

## Value

For `activate()` an object of class spacetime with the specified context
activated. `active()` returns a scalar character with the active context
can be either "goemetry" or "data".

## Details

A
[`spacetime`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
object contains both a data frame and an sf object. The data frame
represents geographies over one or more time periods and the sf object
contains the geographic information for those locations.

## Examples

``` r
df_fp <- system.file("extdata", "bos-ecometric.csv", package = "sfdep")
geo_fp <- system.file("extdata", "bos-ecometric.geojson", package = "sfdep")

# read in data
df <- read.csv(
  df_fp, colClasses = c("character", "character", "integer", "double", "Date")
)
geo <- sf::st_read(geo_fp)
#> Reading layer `bos-ecometric' from data source 
#>   `/home/runner/work/_temp/Library/sfdep/extdata/bos-ecometric.geojson' 
#>   using driver `GeoJSON'
#> Simple feature collection with 168 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.19115 ymin: 42.22788 xmax: -70.99445 ymax: 42.3974
#> Geodetic CRS:  NAD83

# Create spacetime object called `bos`
bos <- spacetime(df, geo,
                 .loc_col = ".region_id",
                 .time_col = "time_period")

active(bos)
#> [1] "data"
activate(bos, "geometry")
#> spacetime ────
#> Context:`geometry`
#> 168 locations `.region_id`
#> 10 time periods `time_period`
#> ── geometry context ────────────────────────────────────────────────────────────
#> Simple feature collection with 168 features and 1 field
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -71.19115 ymin: 42.22788 xmax: -70.99445 ymax: 42.3974
#> Geodetic CRS:  NAD83
#> First 10 features:
#>     .region_id                       geometry
#> 1  25025010405 POLYGON ((-71.09009 42.3466...
#> 2  25025010404 POLYGON ((-71.09066 42.3397...
#> 3  25025010801 POLYGON ((-71.08159 42.3537...
#> 4  25025010702 POLYGON ((-71.07066 42.3518...
#> 5  25025010204 POLYGON ((-71.10683 42.3487...
#> 6  25025010802 POLYGON ((-71.08159 42.3537...
#> 7  25025010104 POLYGON ((-71.08784 42.3474...
#> 8  25025000703 POLYGON ((-71.12622 42.3504...
#> 9  25025000504 POLYGON ((-71.14175 42.3404...
#> 10 25025000704 POLYGON ((-71.13551 42.3487...
```
