# Modify object classes

Appends classes to exist object classes. This is utilized to aid in
adding a list class to objects created by spdep. This enables to use of
the returned objects within data frames and tibbles.

## Usage

``` r
class_modify(x, class = "list")
```

## Arguments

- x:

  an object to modify

- class:

  a character vector of classes to append to an object
