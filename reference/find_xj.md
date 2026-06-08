# Identify xj values

Find `xj` values given a numeric vector, `x`, and neighbors list, `nb`.

## Usage

``` r
find_xj(x, nb)
```

## Arguments

- x:

  a vector of any class

- nb:

  a `nb` object e.g. created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)
  or
  [`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md)

## Value

A list of length `x` where each element is a numeric vector with the
same length as the corresponding element in `nb`.

## Examples

``` r
nb <- st_contiguity(sf::st_geometry(guerry))
xj <- find_xj(guerry$crime_prop, nb)
xj[1:3]
#> [[1]]
#> [1]  8326  8059  4504 10708
#> 
#> [[2]]
#> [1] 8847 4950 6175 6659 5786 7144
#> 
#> [[3]]
#> [1] 10503 20235 12665  8236 12141 10708
#> 
```
