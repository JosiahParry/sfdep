# Pairwise CLQ calculation

Internal function to calculate the pairwise CQL.

## Usage

``` r
pairwise_colocation_calc(A, B, nb)
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

## Value

a matrix where rownames are A values colnames are B values
