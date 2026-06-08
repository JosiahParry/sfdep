# Pairwise CLQ conditional permutation implementation

Internal implementation of the pairwise CQL using conditional
permutation. Functionally the same as
[`pairwise_colocation()`](https://josiahparry.github.io/sfdep/reference/pairwise_colocation.md).

## Usage

``` r
pairwise_colocation_perm_impl(A, B, nb, nsim = 199)
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

a matrix used by pairwise_colocation
