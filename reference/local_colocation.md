# Local indicator of Colocation Quotient

The local indicator of the colocation quotient (LCLQ) is a Local
Indicator of Spatial Association (LISA) that evaluates if a given
observation's subcategory in A is colocated with subcategories in B.
Like the CLQ, the LCLQ provides insight into the asymmetric
relationships between subcategories of A and B (where B can also equal
A) but at the local level.

The LCLQ is defined using Gaussian kernel weights and an adaptive
bandwidth (see
[`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md)).
However, any type of weights list can be used. Kernel weights are used
to introduce a decay into the calculation of the CLQ. This ensures that
points nearer to the focal point have more influence than those that are
more distant.

## Usage

``` r
local_colocation(A, B, nb, wt, nsim)
```

## Arguments

- A:

  a character or factor vector.

- B:

  a character or factor vector.

- nb:

  a neighbors list e.g. created by
  [`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md)
  or
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)

- wt:

  a weights list. Recommended that it is a Gaussian kernel weights list
  using an adaptive bandwidth e.g. created by
  `st_kernel_weights(nb, geometry, "gaussian", addaptive = TRUE)` that
  does not include the self.

- nsim:

  default `99`. An integer representing how many simulations to run for
  calculating the simulated p-values.

## Value

a data frame with as many rows as observations in A and two times as
many columns as unique values in B. Columns contain each unique value of
B as well as the simulated p-value for each value of B.

## Details

The LCLQ is defined as \\LCLQ\_{A_i \to B} = \frac{N\_{A_i \to B}}{N_B /
(N - 1)}\\ where \\N\_{A_i \to B} = \sum\_{j = 1(j \ne
i)}^{N}(\frac{w\_{ij}f\_{ij}}{\sum\_{j = 1(j \ne i)}^{N}w\_{ij}})\\. And
the weights matrix, wij, uses adaptive bandwidth Gaussian kernel
weights.

LCLQ is only calculated for those subcategories which are present in the
neighbor list. If a subcategory is not present, then the resultant LCLQ
and simulated p-value will be `NA`.

## References

Fahui Wang, Yujie Hu, Shuai Wang & Xiaojuan Li (2017) Local Indicator of
Colocation Quotient with a Statistical Significance Test: Examining
Spatial Association of Crime and Facilities, The Professional
Geographer, 69:1, 22-31,
[doi:10.1080/00330124.2016.1157498](https://doi.org/10.1080/00330124.2016.1157498)

## Examples

``` r
A <- guerry$main_city
B <- guerry$region
geo <- sf::st_centroid(sf::st_geometry(guerry))
nb <- include_self(st_knn(geo, 5))
wt <- st_kernel_weights(nb, geo, "gaussian", adaptive = TRUE)
res <- local_colocation(A, B, nb, wt, 9)
tail(res)
#>           C        E         N        S        W p_sim_C p_sim_E p_sim_N
#> 80       NA 1.770957        NA 3.170219       NA      NA     0.3      NA
#> 81       NA       NA        NA       NA 4.941176      NA      NA      NA
#> 82 2.564959       NA        NA       NA 2.376218     0.2      NA      NA
#> 83 2.478538       NA        NA       NA 2.462639     0.2      NA      NA
#> 84       NA 4.186106 0.7550705       NA       NA      NA     0.1     0.1
#> 85 2.321401 1.816223 0.8035523       NA       NA     0.2     0.1     0.3
#>    p_sim_S p_sim_W
#> 80     0.1      NA
#> 81      NA     0.1
#> 82      NA     0.1
#> 83      NA     0.2
#> 84      NA      NA
#> 85      NA      NA
```
