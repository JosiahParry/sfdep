# Create time lagged spatial neighbors

Given a an spdep neighbor list (or any other list indicating row
position) of neighbors in a `spacetime` object's geometry context,
create a neighbor list across space and time in the `data` context.

## Usage

``` r
spt_nb(nb, n_times, n_locs, k = 1)
```

## Arguments

- nb:

  a neighbors list created from a spacetime object's geometry context.

- n_times:

  the number of time slices

- n_locs:

  the number of locations

## Value

a `nb` list

## Details

Note that this should only be used for spacetime cubes.

This function is only available for internal development and should be
used with extreme caution.

The number of bins is equal to `n_times * n_locs` where each location
has a bin time-series with `n_times` observations. This is what makes it
a spacetime cube function.

## References

https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/create-space-time-cube.htm

## See also

[`spt_wt()`](https://josiahparry.github.io/sfdep/reference/spt_wt.md)
for converting spacetime neighbors to spacetime weights.
