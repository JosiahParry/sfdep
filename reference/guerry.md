# "Essay on the Moral Statistics of France" data set.

This dataset has been widely used to demonstrate geospatial methods and
techniques. As such it is useful for inclusion to this R package for the
purposes of example. The dataset in this package is modified from Guerry
by [Michael Friendly](https://www.datavis.ca/).

## Usage

``` r
guerry

guerry_nb
```

## Format

An object of class `sf` (inherits from `tbl_df`, `tbl`, `data.frame`)
with 85 rows and 27 columns.

`guerry` an sf object with 85 observations and 27 variables. `guerry_nb`
has 2 additional variables created by `sfdep`.

## Source

`Guerry::gfrance85`

## Details

`guerry` and `guerry_nb` objects are sf class objects. These are
polygons of the boundaries of France (excluding Corsica) as they were in
1830.
