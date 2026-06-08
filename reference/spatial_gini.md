# Spatial Gini Index

Calculates the spatial Gini index for a given numeric vector and
neighbor list. Based on the formula provided Rey and Smith (2013).

## Usage

``` r
spatial_gini(x, nb)
```

## Arguments

- x:

  a numeric vector without missing values

- nb:

  a neighbor list, for example created with
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)

## Value

A data frame with columns:

- `G`: the Gini index

- `NBG`: the neighbor composition of the Gini coefficient

- `NG`: the non-neighbor composition of the Gini coefficient

- `SG`: the Spatial Gini which is equal to \\NG \* \frac{1}{G}\\

## Details

The Gini index is a global measure of inequality based on the Lorenz
curve. It ranges between 0 and 1 where 0 is perfect equality and 1 is
perfect inequality.

The spatial Gini index decomposes the Gini coefficient based on spatial
neighbors.

## References

[doi:10.1007/s12076-012-0086-z](https://doi.org/10.1007/s12076-012-0086-z)

## Examples

``` r
nb <- st_contiguity(guerry)
x <- guerry$wealth
spatial_gini(x, nb)
#>           G        NG       NBG        SG
#> 1 0.3306568 0.3164337 0.0142231 0.9569853
```
