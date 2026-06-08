# Kernel functions

Kernel functions for creating kernel based weights with
[`st_kernel_weights()`](https://josiahparry.github.io/sfdep/reference/st_kernel_weights.md)

## Usage

``` r
kernels
```

## Format

An object of class `list` of length 5.

## Value

a list of length 5 where each element is a kernel function.

## Details

Supported kernels are below.

Formulas come from Anselin & Morrison's
[notes](https://spatialanalysis.github.io/lab_tutorials/Spatial_Weights_as_Distance_Functions.html#kernal-weights).

- `uniform`: K(z) = 1/2 for \|z\|\<1

- `triangular`: K(z) = (1-\|z\|) for \|z\| \< 1

- `epanechnikov`: K(z) = (3/4)(1-z^2) for \|z\| \< 1

- `quartic`: K(z) = (15/16)\*(1-(z/threshold)^2^)2 for \|z\| \< 1

- `gaussian`: K(z) = (2pi)^{1/2} \* exp(-z^2/2)
