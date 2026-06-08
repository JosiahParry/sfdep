# Create time lagged spatial weights

Given a space-time-lagged neighbor list and a weights lists, e.g. made
by
[`st_dist_band()`](https://josiahparry.github.io/sfdep/reference/st_dist_band.md)
or
[`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md),
create a corresponding weights list.

## Usage

``` r
spt_wt(wt, nbt, n_times, n_locs, k)
```

## Arguments

- wt:

  a weights lists created from a spacetime object's geometry context.

- nbt:

  a time-lagged spatial neighbors list created by
  [`spt_nb()`](https://josiahparry.github.io/sfdep/reference/spt_nb.md)

- n_times:

  the number of time slices

- n_locs:

  the number of locations

## Value

a weight list with same cardinality as `nbt`

## Details

It is intended that `spt_wt()` is used following the use of
[`spt_nb()`](https://josiahparry.github.io/sfdep/reference/spt_nb.md)
using the same input arguments. This ensures that the time-lagged
spatial weights are correctly associated with the time-lagged spatial
neighbors.

## See also

[`spt_nb()`](https://josiahparry.github.io/sfdep/reference/spt_nb.md)
for creating spacetime neighbors which are inputs into spacetime weights
