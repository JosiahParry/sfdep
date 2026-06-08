# Calculate Kernel Weights

Create a weights list using a kernel function.

## Usage

``` r
st_kernel_weights(
  nb,
  geometry,
  kernel = "uniform",
  threshold = critical_threshold(geometry),
  adaptive = FALSE,
  self_kernel = FALSE
)
```

## Arguments

- nb:

  an object of class `nb` e.g. created by
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md)
  or
  [`st_knn()`](https://josiahparry.github.io/sfdep/reference/st_knn.md).

- geometry:

  the geometry an sf object.

- kernel:

  One of "uniform", "gaussian", "triangular", "epanechnikov", or
  "quartic". See
  [kernels](https://josiahparry.github.io/sfdep/reference/kernels.md)
  for more.

- threshold:

  a scaling threshold to be used in calculating

- adaptive:

  default `FALSE`. If `TRUE` uses the maximum neighbor distance for each
  region as the threshold. Suppresses the `threshold` argument.

- self_kernel:

  default `FALSE`. If `TRUE` applies the kernel function to the observed
  region.

## Value

a list where each element is a numeric vector.

## Details

By default `st_kernel_weight()` utilizes a critical threshold of the
maximum neighbor distance using
[`critical_threshold()`](https://josiahparry.github.io/sfdep/reference/critical_threshold.md).
If desired, the critical threshold can be specified manually. The
`threshold` will be passed to the underlying kernel.

## See also

Other weights:
[`st_inverse_distance()`](https://josiahparry.github.io/sfdep/reference/st_inverse_distance.md),
[`st_nb_dists()`](https://josiahparry.github.io/sfdep/reference/st_nb_dists.md),
[`st_weights()`](https://josiahparry.github.io/sfdep/reference/st_weights.md)

## Examples

``` r
geometry <- sf::st_geometry(guerry)
nb <- st_contiguity(geometry)
nb <- include_self(nb)
res <- st_kernel_weights(nb, geometry)
#> ! Polygon provided. Using point on surface.
head(res, 3)
#> [[1]]
#> [1] 1.0 0.5 0.5 0.5 0.5
#> 
#> [[2]]
#> [1] 1.0 0.5 0.5 0.5 0.5 0.5 0.5
#> 
#> [[3]]
#> [1] 1.0 0.5 0.5 0.5 0.5 0.5 0.5
#> 
```
