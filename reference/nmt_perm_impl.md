# Find conditionally permuted neighbor matches Given a kNN attribute neighbor list and a listw object, find the number of matches given a conditional permutation.

Find conditionally permuted neighbor matches Given a kNN attribute
neighbor list and a listw object, find the number of matches given a
conditional permutation.

## Usage

``` r
nmt_perm_impl(knn_nb, listw)
```

## Arguments

- knn_nb:

  a list with numeric elements. For example as made by
  [`dbscan::adjacencylist()`](https://rdrr.io/pkg/dbscan/man/NN.html)

- listw:

  a listw object likely created by
  [recreate_listw](https://josiahparry.github.io/sfdep/reference/recreate_listw.md).

## Value

an integer vector
