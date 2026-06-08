# Set columns from `geometry` to `data`

Set a column from the `geometry` context of a spacetime object to the
`data` context.

## Usage

``` r
set_col(x, .from_geo, .to_data = .from_geo)

set_wts(x, .wt_col = "wt")

set_nbs(x, .nb_col = "nb")
```

## Arguments

- x:

  a spacetime object

- .from_geo:

  the name of the column in the `geometry` context

- .to_data:

  the name of the new variable to create in the `data` context

- .wt_col:

  the name of the weights column in the `geometry` context

- .nb_col:

  the name of neighbor column in the `geometry` context

## Value

A spacetime object with an active data context and a new column from the
geometry context.

## Details

These functions will reorder the spacetime object to ensure that it is
ordered correctly based on the location time columns in the `geometry`
context defined by the `loc_col` and `time_col` attributes respectively.

`set_wts()` and `set_nbs()` create a new column in the data context with
the same name as the column in the geometry context. If a different name
is desired use `set_col()`

## Examples

``` r

if (interactive()) {
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
bos <- activate(bos, "geometry")
bos$nb <- st_contiguity(bos)
bos$wt <- st_weights(bos$nb)
bos$card <- st_cardinalties(bos$nb)

set_nbs(bos)
set_wts(bos)
set_col(bos, "card")
set_col(bos, "card", "cardinalities")
}
```
