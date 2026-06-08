# Colocation Quotients CLQs

``` r

library(sfdep)
library(dplyr)
```

The colocation quotient evaluates if there is a spatial association
between two categorical vectors $`A`$ and $`B`$. It is similar to the
join count and cross-K metric. It’s unique, however, in the way that the
CLQ can measure asymmetric relationships.

The CLQ is a ratio of the observed proportion of a categorical variable
to the expected proportion. Values larger than 1 mean that there is more
colocation than to be expected under the null hypothesis of no spatial
association. When the value is smaller than 0, we interpret it to mean
that there is less colocation than expected under the null.

> Note that the CLQ’s intended use is with point data.

### Global Colocation

The
[`global_colocation()`](https://josiahparry.github.io/sfdep/reference/global_colocation.md)
quotient is used to identify if there is *any* colocation in a single
categorical vector $`A`$.

``` r

A <- guerry[["main_city"]]
nb <- st_contiguity(sf::st_geometry(guerry))
global_colocation(A, nb, 299)
#> $CLQ
#> [1] 1.02841
#> 
#> $p_sim
#> [1] 0.15
```

The result of the global CLQ is approximately 0 and insignificant. We
can interpret this to mean that there is no significant colocation of
“Main Cities.”

### Pairwise Colocation

The pairwise CLQ evaluates colocation between values of categorical
variables $`A`$ and $`B`$ at a global level. The results are
one-directional with $`A \to B`$.

We provide the 2 vectors to the
[`pairwise_colocation()`](https://josiahparry.github.io/sfdep/reference/pairwise_colocation.md)
function and the results is a matrix with unique values of $`A`$ as
column headers and unique values of $`B`$ as row names. There are
additional columns to indicate the simulated p-values of the colocation
quotients.

``` r

A <- guerry$main_city
B <- guerry$region
nb <- st_knn(sf::st_geometry(guerry), 5)
#> ! Polygon provided. Using point on surface.
pairwise_colocation(B, A, nb, 199)
#>        1:Sm     2:Med      3:Lg p_sim_1:Sm p_sim_2:Med p_sim_3:Lg
#> C 0.8894118 1.1250679 0.1976471      0.425       0.005      0.000
#> E 1.8776471 0.8514027 0.9882353      0.005       0.030      0.530
#> N 0.1976471 1.0794570 1.1858824      0.000       0.030      0.285
#> S 1.5811765 0.9122172 0.8894118      0.040       0.140      0.460
#> W 0.6917647 1.0338462 0.9882353      0.180       0.240      0.525
```

If we look at the first row, we see that the colocation of
$`C \to 3{:}Lg`$ is very significant with a value of ~ $`0.20`$. We can
interpret this to mean that there is systematically less colocation of
region C near large main cities.

### Local Colocation

The local CLQ is rather similar to the pairwise CLQ. The key difference
is the weights list that is used in calculating the CLQ. In its
introductory paper, Wang et al. (2017) emphasize that adaptive gaussian
kernel weights be used. This introduces a distance decay where closer
values have more heft than distant ones in calculating the local CLQ.

The result of the local CLQ returns a CLQ and a simulated p-value for
each unique value of B that falls in the neighborhood.

``` r

# Identify A & B variables
A <- guerry$main_city
B <- guerry$region
geo <- sf::st_geometry(guerry)
nb <- st_knn(geo, 5)
#> ! Polygon provided. Using point on surface.
wt <- st_kernel_weights(nb, geo, "gaussian", adaptive = TRUE)
#> ! Polygon provided. Using point on surface.

res <- local_colocation(A, B, nb, wt, 49)

head(res)
#>           C        E        N        S  W p_sim_C p_sim_E p_sim_N p_sim_S
#> 1 0.8461533 4.095023       NA       NA NA    0.08    0.02      NA      NA
#> 2        NA       NA 4.941176       NA NA      NA      NA    0.02      NA
#> 3 4.9411765       NA       NA       NA NA    0.02      NA      NA      NA
#> 4        NA 2.090837       NA 2.850339 NA      NA    0.16      NA    0.08
#> 5        NA 3.268800       NA 1.672377 NA      NA    0.02      NA    0.24
#> 6 1.1554807 1.878544       NA 1.907152 NA    0.32    0.24      NA    0.22
#>   p_sim_W
#> 1      NA
#> 2      NA
#> 3      NA
#> 4      NA
#> 5      NA
#> 6      NA
```

Note that for the first observation we only have a CLQ for `C` and `E`.
This is because the neighborhood of the first observation consists of
only C and E values. We can verify this like so.

``` r

# view first row
res[1,]
#>           C        E  N  S  W p_sim_C p_sim_E p_sim_N p_sim_S p_sim_W
#> 1 0.8461533 4.095023 NA NA NA    0.08    0.02      NA      NA      NA

# find xj values for the B column based on neighbors
# take first observations neighbors
sfdep::find_xj(B, nb)[[1]] 
#> [1] E E C E E
#> Levels: C E N S W
```

It’s also worth noting that we can create the local CLQ in a dplyr pipe
like so

``` r


res_tidy <- guerry |> 
  select(main_city, region) |> 
  mutate(nb = st_knn(geometry, 5),
         wt = st_kernel_weights(nb, geometry, "gaussian", adaptive = TRUE),
         lclq = local_colocation(main_city, region, nb, wt, 99)) |> 
  tidyr::unnest(lclq)
#> ! Polygon provided. Using point on surface.
#> ! Polygon provided. Using point on surface.

slice(res_tidy, 1:5)
#> Simple feature collection with 5 features and 14 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 595532 ymin: 1858801 xmax: 975716 ymax: 2564568
#> CRS:           NA
#> # A tibble: 5 × 15
#>   main_city region                       geometry nb    wt         C     E     N
#>   <ord>     <fct>                  <MULTIPOLYGON> <nb>  <lis>  <dbl> <dbl> <dbl>
#> 1 2:Med     E      (((801150 2092615, 800669 209… <int> <dbl>  0.846  4.10 NA   
#> 2 2:Med     N      (((729326 2521619, 729320 252… <int> <dbl> NA     NA     4.94
#> 3 2:Med     C      (((710830 2137350, 711746 213… <int> <dbl>  4.94  NA    NA   
#> 4 1:Sm      E      (((882701 1920024, 882408 192… <int> <dbl> NA      2.09 NA   
#> 5 1:Sm      E      (((886504 1922890, 885733 192… <int> <dbl> NA      3.27 NA   
#> # ℹ 7 more variables: S <dbl>, W <dbl>, p_sim_C <dbl>, p_sim_E <dbl>,
#> #   p_sim_N <dbl>, p_sim_S <dbl>, p_sim_W <dbl>
```
