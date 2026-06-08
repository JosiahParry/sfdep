# Bivariate local join count

Bivariate local join count

## Usage

``` r
local_jc_bv(x, z, nb, wt, nsim = 499)
```

## Arguments

- x:

  a binary variable either numeric or logical

- z:

  a binary variable either numeric or logical

- nb:

  a neighbors list object.

- wt:

  default `st_weights(nb, style = "B")`. A binary weights list as
  created by `st_weights(nb, style = "B")`.

- nsim:

  the number of conditional permutation simulations

## Value

a `data.frame` with two columns `join_count` and `p_sim` and number of
rows equal to the length of arguments `x`, `z`, `nb`, and `wt`.

## Examples

``` r
x <- as.integer(guerry$infants > 23574)
z <- as.integer(guerry$donations > 10973)
nb <- st_contiguity(guerry)
wt <- st_weights(nb, style = "B")
local_jc_bv(x, z, nb, wt)
#>    join_count p_sim
#> 1           0    NA
#> 2           0    NA
#> 3           0    NA
#> 4           0    NA
#> 5           0    NA
#> 6           0    NA
#> 7           0    NA
#> 8           0    NA
#> 9           0    NA
#> 10          0    NA
#> 11          0    NA
#> 12          0    NA
#> 13          0    NA
#> 14          0    NA
#> 15          0    NA
#> 16          2 0.010
#> 17          0    NA
#> 18          0    NA
#> 19          0    NA
#> 20          0    NA
#> 21          0    NA
#> 22          0    NA
#> 23          0    NA
#> 24          0    NA
#> 25          0    NA
#> 26          0    NA
#> 27          1 0.122
#> 28          0    NA
#> 29          0    NA
#> 30          0    NA
#> 31          0    NA
#> 32          0    NA
#> 33          0    NA
#> 34          0    NA
#> 35          0    NA
#> 36          0    NA
#> 37          0    NA
#> 38          0    NA
#> 39          0    NA
#> 40          0    NA
#> 41          0    NA
#> 42          0    NA
#> 43          0    NA
#> 44          0    NA
#> 45          0    NA
#> 46          0    NA
#> 47          0    NA
#> 48          0    NA
#> 49          0    NA
#> 50          0    NA
#> 51          0    NA
#> 52          0    NA
#> 53          0    NA
#> 54          1 0.180
#> 55          0    NA
#> 56          0    NA
#> 57          0    NA
#> 58          0    NA
#> 59          0    NA
#> 60          0    NA
#> 61          0    NA
#> 62          0    NA
#> 63          0    NA
#> 64          0    NA
#> 65          0    NA
#> 66          0    NA
#> 67          0    NA
#> 68          0    NA
#> 69          0    NA
#> 70          0    NA
#> 71          0    NA
#> 72          0    NA
#> 73          0    NA
#> 74          0    NA
#> 75          2 0.018
#> 76          0    NA
#> 77          0    NA
#> 78          0    NA
#> 79          0    NA
#> 80          0    NA
#> 81          2 0.004
#> 82          0    NA
#> 83          0    NA
#> 84          0    NA
#> 85          0    NA
```
