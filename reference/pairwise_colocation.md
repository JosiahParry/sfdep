# Pairwise Colocation Quotient

Calculate the pairwise colocation quotient (CLQ) for two categorical
variables using conditional permutation.

## Usage

``` r
pairwise_colocation(A, B, nb, nsim = 99)
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

- nsim:

  default `99`. An integer representing how many simulations to run for
  calculating the simulated p-values.

## Value

A matrix where the rownames are the unique values of A and the column
names are the unique values of B and their simulated p-values in the
form of `p_sim_{B}`.

## Details

### Intuition

The pairwise CLQ is used to test if there is a spatial directional
association between subcategories of two vectors `A` and `B`. Compared
to the cross-K metric and the join count statistic, the pairwise CLQ can
elucidate the presence of an asymmetric relationship between
subcategories of A and B. A and B can either be separate categorical
vectors or the same categorical vector.

"The null hypothesis for a CLQ-based analysis is 'given the clustering
of the joint population, there is no spatial association between pairs
of categorical subsets.'"

### Definition

The pairwise colocation quotient is defined as "the ratio of observed to
expected proportions of B among A's nearest neighbors. Formally this is
given by \\CLQ\_{A \to B} = \frac{{C\_{A \to B} / N_A}}{N^{'}\_{B} /
(N - 1)}\\" where \\C\_{A \to B} = \sum\_{i = 1}^{N_A}\sum\_{j =
1}^{v}\frac{B\_{ij}(1,0)}{v}\\.

### Inference

Inference is done using conditional permutation as suggested by Anselin
1995 where a number of replicates are created. The observed values are
compared to the replicates and a the simulated p-value is the proportion
of cases where the observed is more extreme as compared to replicate.
The simulated p-value returns the lower p-value of either tail.

### Interpretation

Given that the CLQ is a ratio of the observed to expected, we interpret
values larger than one to mean that there is more colocation than to be
expected under the null hypothesis of no spatial association. When the
value is smaller than 0, we interpret it to mean that there is less
colocation than expected under the null.

## Examples

``` r
A <- guerry$main_city
B <- guerry$region
nb <- st_knn(sf::st_geometry(guerry), 5)
#> ! Polygon provided. Using point on surface.
pairwise_colocation(B, A, nb)
#>        1:Sm     2:Med      3:Lg p_sim_1:Sm p_sim_2:Med p_sim_3:Lg
#> C 0.8894118 1.1250679 0.1976471       0.46        0.00       0.00
#> E 1.8776471 0.8514027 0.9882353       0.02        0.01       0.53
#> N 0.1976471 1.0794570 1.1858824       0.00        0.05       0.19
#> S 1.5811765 0.9122172 0.8894118       0.01        0.05       0.47
#> W 0.6917647 1.0338462 0.9882353       0.20        0.25       0.49
pairwise_colocation(B, B, nb, 199)
#>           C         E         N         S         W p_sim_C p_sim_E p_sim_N
#> C 3.5205882 0.3705882 0.4941176 0.3088235 0.5558824   0.000   0.000    0.00
#> E 0.4323529 3.8294118 0.4941176 0.4941176 0.0000000   0.005   0.000    0.01
#> N 0.4323529 0.5558824 3.9529412 0.0000000 0.3088235   0.000   0.025    0.00
#> S 0.3705882 0.5558824 0.0000000 3.9529412 0.3705882   0.000   0.005    0.00
#> W 0.5558824 0.0000000 0.3088235 0.4941176 3.8911765   0.010   0.000    0.00
#>   p_sim_S p_sim_W
#> C    0.00   0.015
#> E    0.01   0.000
#> N    0.00   0.000
#> S    0.00   0.000
#> W    0.00   0.000
```
