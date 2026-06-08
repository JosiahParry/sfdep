# Global Colocation Quotient

Calculate the Global Colocation Quotient (CLQ) for a categorical
variable using simulation based significance testing.

## Usage

``` r
global_colocation(A, nb, nsim = 99)
```

## Arguments

- A:

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

A list of two elements `CLQ` and `p_sim` containing the observed
colocation quotient and the simulated p-value respectively.

## Details

### Definition

The CLQ is defined as \\CLQ\_{Global} = \frac{\sum\_{A \in X} C\_{A \to
A}}{\sum\_{A \in X} N_A ({\frac{N_A - 1}{N-1})}}\\. The numerator
identifies the observed proportion of same-category neighbors while the
denominator contains the *expected* proportion of same-category
neighbors under the assumption of no spatial association. Thus the CLQ
is just a ratio of observed to expected.

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

## References

Leslie, T.F. and Kronenfeld, B.J. (2011), The Colocation Quotient: A New
Measure of Spatial Association Between Categorical Subsets of Points.
Geographical Analysis, 43: 306-326.
[doi:10.1111/j.1538-4632.2011.00821.x](https://doi.org/10.1111/j.1538-4632.2011.00821.x)

## Examples

``` r
A <- guerry$main_city
nb <- st_contiguity(sf::st_geometry(guerry))
global_colocation(A, nb, 49)
#> $CLQ
#> [1] 1.02841
#> 
#> $p_sim
#> [1] 0.14
#> 
```
