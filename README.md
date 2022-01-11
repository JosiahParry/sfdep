# sfdep

`sfdep` is a tidy translation of `spdep`. And a cleaner rewrite of `sfweight` .

sfdep maintains classes created by spdep when possible.

# 

### Notes between sfweight

-   neighbors

    -   contiguity

        -   `st_contiguity()` now maintains `nb` class

    -   nb lags

    -   cumulative lag

-   weights

    -   given that `nb` class is maintained, no need to cast

-   lag

## TODO:

[] implement localC

[] implement kernel weights

[] implement neighbor match test

[] Document Kernels

[] implement tests

[] write plotting / mapping functions

[] vastly improve documentation.

[] use s2 for nbdists with polygons
