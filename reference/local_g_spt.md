# Calculate the Local Gi\* for a spacetime cube

An alternative implementation to
[spdep::localG](https://r-spatial.github.io/spdep/reference/localG.html)
intended for use with a spacetime cube.

## Usage

``` r
local_g_spt(x, times, nb, wt, n_locs, nsim)
```

## Arguments

- x:

  a numeric vector

- times:

  a vector determining time order

- nb:

  a spacetime neighbors list

- wt:

  a weights list associated with `nb`

- n_locs:

  the number of unique locations

- nsim:

  the number of simulations to run for calculating `p_sim`
