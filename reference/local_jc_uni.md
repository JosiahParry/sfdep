# Compute local univariate join count

The univariate local join count statistic is used to identify clusters
of rarely occurring binary variables. The binary variable of interest
should occur less than half of the time.

## Usage

``` r
local_jc_uni(
  fx,
  chosen,
  nb,
  wt = st_weights(nb, style = "B"),
  nsim = 499,
  alternative = "two.sided",
  iseed = NULL
)
```

## Arguments

- fx:

  a binary variable either numeric or logical

- chosen:

  a scalar character containing the level of `fx` that should be
  considered the observed value (1).

- nb:

  a neighbors list object.

- wt:

  default `st_weights(nb, style = "B")`. A binary weights list as
  created by `st_weights(nb, style = "B")`.

- nsim:

  the number of conditional permutation simulations

- alternative:

  default `"greater"`. One of `"less"` or `"greater"`.

- iseed:

  default NULL, used to set the seed; the output will only be
  reproducible if the count of CPU cores across which computation is
  distributed is the same

## Value

a `data.frame` with two columns `join_count` and `p_sim` and number of
rows equal to the length of arguments `x`, `nb`, and `wt`.

## Details

The local join count statistic requires a binary weights list which can
be generated with `st_weights(nb, style = "B")`. Additionally, ensure
that the binary variable of interest is rarely occurring in no more than
half of observations.

P-values are estimated using a conditional permutation approach. This
creates a reference distribution from which the observed statistic is
compared. For more see [Geoda
Glossary](https://geodacenter.github.io/glossary.html#ppvalue). Calls
[`spdep::local_joincount_uni()`](https://r-spatial.github.io/spdep/reference/local_joincount_uni.html).

## Examples

``` r

if (requireNamespace("dplyr", quietly = TRUE)) {

res <- dplyr::transmute(
  guerry,
  top_crime = as.factor(crime_prop > 9000),
  nb = st_contiguity(geometry),
  wt = st_weights(nb, style = "B"),
  jc = local_jc_uni(top_crime, "TRUE", nb, wt))
tidyr::unnest(res, jc)

}
#> Simple feature collection with 85 features and 12 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 85 × 13
#>    top_crime nb        wt           BB `Pr(z != E(BBi))` sim_rank p_sim_pysal_ge
#>    <fct>     <nb>      <list>    <dbl>             <dbl>    <int>          <dbl>
#>  1 TRUE      <int [4]> <dbl [4]>     1             0.924      231          0.25 
#>  2 FALSE     <int [6]> <dbl [6]>     0            NA           NA         NA    
#>  3 FALSE     <int [6]> <dbl [6]>     0            NA           NA         NA    
#>  4 FALSE     <int [4]> <dbl [4]>     0            NA           NA         NA    
#>  5 FALSE     <int [3]> <dbl [3]>     0            NA           NA         NA    
#>  6 TRUE      <int [7]> <dbl [7]>     2             0.944      265          0.368
#>  7 FALSE     <int [3]> <dbl [3]>     0            NA           NA         NA    
#>  8 TRUE      <int [3]> <dbl [3]>     1             0.876      282          0.326
#>  9 FALSE     <int [5]> <dbl [5]>     0            NA           NA         NA    
#> 10 TRUE      <int [5]> <dbl [5]>     2             0.608      349          0.456
#> # ℹ 75 more rows
#> # ℹ 6 more variables: p_sim_pysal_gt <dbl>, largereq <dbl>, larger <dbl>,
#> #   olargereq <int>, olarger <int>, geometry <MULTIPOLYGON>
```
