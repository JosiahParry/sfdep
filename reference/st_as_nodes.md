# Convert to a node point object

Given geometry and a neighbor list, creates an `sf` object to be used as
nodes in an
[`sfnetworks::sfnetwork()`](https://luukvdmeer.github.io/sfnetworks/reference/sfnetwork.html).
If the provided geometry is a polygon,
[`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html)
will be used to create the node point.

## Usage

``` r
st_as_nodes(x, nb)

# S3 method for class 'sf'
st_as_nodes(x, nb)

# S3 method for class 'sfc'
st_as_nodes(x, nb)
```

## Arguments

- x:

  object of class `sf` or `sfc`.

- nb:

  a neighbor list. If `x` is class `sf`, the unquote named of the
  column. If `x` is class `sfc`, an object of class `nb` as created from
  [`st_contiguity()`](https://josiahparry.github.io/sfdep/reference/st_contiguity.md).

## Value

An object of class `sf` with `POINT` geometry.

## Details

`st_as_node()` adds a row `i` based on the attribute `"region.id"` in
the `nb` object. If the `nb` object is created with `sfdep`, then the
values will always be row indexes.

## Examples

``` r
if (requireNamespace("dplyr", quitly = TRUE)) {
library(magrittr)
guerry %>%
  dplyr::transmute(nb = st_contiguity(geometry)) %>%
  st_as_nodes(nb)
 }
```
