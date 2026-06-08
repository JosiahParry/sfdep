# Update spacetime attributes

Update's a spacetime object's number of locations and time periods. A
spacetime object's attributes are sticky and will not change if
subsetted for example by using
[`dplyr::filter()`](https://dplyr.tidyverse.org/reference/filter.html)
or [`dplyr::slice()`](https://dplyr.tidyverse.org/reference/slice.html).
Update the locations and times of a spacetime object.

## Usage

``` r
spt_update(x, ...)
```

## Arguments

- x:

  a spacetime object

- ...:

  unused

## Value

an object of class spacetime with updated attributes
