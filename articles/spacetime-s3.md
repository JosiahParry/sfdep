# spacetime and spacetime cubes

``` r

library(sfdep)
library(dplyr)
```

sfdep introduces a new s3 class to represent spatio-temporal data. The
`spacetime` class links a flat data set containing spatio-temporal
information with the related geometry. The spacetime class is informed
by the [spacetime](https://github.com/edzer/spacetime) package by Edzer
Pebesma (2012), and the interface is inspired by the design of
[tidygraph](https://tidygraph.data-imaginist.com).

## Spatio-temporal data

Traditionally “spatio-temporal data often come in the form of single
tables” that can typically be categorized as “time-wide”, “space-wide”,
or “long formats.” In long formats, often referred to as “tidy”, a row
identifies a unique location and time observation represented by a
column dedicated to time and another to locations. This is the typical
presentation of panel data.

Space-wide data present each time period across each row and locational
information in each column. Whereas a time-wide representation has
location data down the rows and each time period is represented as a new
column.

These flat formats are not linked to the geographies that they represent
in any meaningful way. These flat files typically contain only an
identifier of the location, but the spatial representation.

The `spacetime` class is developed with particular focus to lattice
data. That is, to create a representation of spatio-temporal data for a
set of regions over a number of different time-periods e.g. population
densities in census tracts for each year.

To represent spatial data in a temporal context Pebesma, 2012 identifies
a number of spatio-temporal layouts, two of which are of particular
interest. These are the spatio-temporal full grid and sparse grids.

Given a number of spatial features $`n`$, and time periods $`m`$, a
*spatio-temporal full grid* contains $`n \times m`$ rows. Each location
has a recorded observation for each of the time periods in $`m`$. For
example, if there are 10 locations and 20 time periods, there are 20
observations per location meaning there are $`10 \times 20 = 200`$
observations. This is efficient only when are there are complete
time-series for each location.

When there are missing observations for some locations or time periods
and they are entirely omitted from the data set, that is a
*spatio-temporal sparse grid*. In this case $`N \lt m \times n`$

## spacetime s3 class in sfdep

Inspired by the design of the tidygraph package, the spacetime class
links a data frame and an sf object based on a shared location
identifier column. These are referred to as the *data context* and the
*geometry context*. The spacetime class allows you switch between
different contexts and work with them individually as you see fit.

Typically, if one wants to represent location data over multiple time
periods containing information about the geography, an sf object is used
which duplicates the geometry at each location for each time period
which can be computationally expensive. By linking sf objects to a data
frame based on their location ID, we are able to avoid this problem

There are four important aspects to the spacetime class:

- the data: a `data.frame` object
- the geometry: an `sf` object
- the location identifiers
- the time column

### Creating a spacetime object

There are two ways to create spacetime objects: 1) with
[`as_spacetime()`](https://josiahparry.github.io/sfdep/reference/as_spacetime.md)
and 2)
[`spacetime()`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
or
[`new_spacetime()`](https://josiahparry.github.io/sfdep/reference/spacetime.md).
The former takes an sf object that contains the location IDs, times, and
geometry and converts it into a spacetime object. Whereas the
constructor functions require a data frame and a separate sf object
containing the geometry.

Let’s create a sample data set using the `guerry`

``` r

# replicate the guerry dataset 10 times
x <- purrr::map_dfr(1:10, ~guerry) |> 
  select(code_dept, crime_pers) |> 
         # create an indicator for time period
  mutate(time_period = sort(rep(1:10, 85)), 
         # add some noise 
         crime_pers = crime_pers * runif(850, max = 2))

x
#> Simple feature collection with 850 features and 3 fields
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 850 × 4
#>    code_dept crime_pers                                     geometry time_period
#>  * <fct>          <dbl>                               <MULTIPOLYGON>       <int>
#>  1 01             4663. (((801150 2092615, 800669 2093190, 800688 2…           1
#>  2 02            43762. (((729326 2521619, 729320 2521230, 729280 2…           1
#>  3 03            32137. (((710830 2137350, 711746 2136617, 712430 2…           1
#>  4 04             4067. (((882701 1920024, 882408 1920733, 881778 1…           1
#>  5 05              259. (((886504 1922890, 885733 1922978, 885479 1…           1
#>  6 07             8837. (((747008 1925789, 746630 1925762, 745723 1…           1
#>  7 08            35047. (((818893 2514767, 818614 2514515, 817900 2…           1
#>  8 09             3577. (((509103 1747787, 508820 1747513, 508154 1…           1
#>  9 10            28732. (((775400 2345600, 775068 2345397, 773587 2…           1
#> 10 11            24175. (((626230 1810121, 626269 1810496, 627494 1…           1
#> # ℹ 840 more rows
```

This representation, where there are duplicate geometries for each
location, should be cast into a spacetime object using
[`as_spacetime()`](https://josiahparry.github.io/sfdep/reference/as_spacetime.md).

``` r

spt <- as_spacetime(x, "code_dept", "time_period")
```

Alternatively, we have the other scenario, where we have the geometry
and the data as two separate objects. In this case we can use the
[`spacetime()`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
constructor. It’s required arguments are `.data`, .`geometry`,
`.loc_col`, `.time_col`. `.data` must be a data frame and `.geometry`
must be a tibble.

Here we create a data frame `df` which contains columns for the location
identifier, the time period, and any other variables of interest in this
case `crime_pers`.

``` r

df <- sf::st_drop_geometry(x)
geo <- select(guerry, code_dept)

head(df)
#> # A tibble: 6 × 3
#>   code_dept crime_pers time_period
#>   <fct>          <dbl>       <int>
#> 1 01             4663.           1
#> 2 02            43762.           1
#> 3 03            32137.           1
#> 4 04             4067.           1
#> 5 05              259.           1
#> 6 07             8837.           1
```

Note that the location identifier column is the same between the two
objects—this is a requirement.

``` r

spt <- spacetime(
  .data = df, 
  .geometry = geo, 
  .loc_col = "code_dept", 
  .time_col = "time_period"
  ) 

spt
#> spacetime ────
#> Context:`data`
#> 85 locations `code_dept`
#> 10 time periods `time_period`
#> ── data context ────────────────────────────────────────────────────────────────
#> # A tibble: 850 × 3
#>    code_dept crime_pers time_period
#>  * <fct>          <dbl>       <int>
#>  1 01             4663.           1
#>  2 02            43762.           1
#>  3 03            32137.           1
#>  4 04             4067.           1
#>  5 05              259.           1
#>  6 07             8837.           1
#>  7 08            35047.           1
#>  8 09             3577.           1
#>  9 10            28732.           1
#> 10 11            24175.           1
#> # ℹ 840 more rows
```

> As an aside, I’d note that
> [`as_spacetime()`](https://josiahparry.github.io/sfdep/reference/as_spacetime.md)
> uses the sf distinct method which can be a bit computationally intense
> depending on your geometries. As such I’d recommend using
> [`spacetime()`](https://josiahparry.github.io/sfdep/reference/spacetime.md)
> constructor always.

With the spacetime objects, we can also cast them back into sf objects
using `as_sf(x)`.

## Spacetime Contexts

Spacetime objects have two contexts: the data and geometry contexts.

The **data context** consists of a data frame object. It can be
manipulated just like any other data frame. You switch between contexts
using
[`activate()`](https://josiahparry.github.io/sfdep/reference/activate.md).
To switch to the data context activate “data.”

``` r

activate(spt, "data")
#> spacetime ────
#> Context:`data`
#> 85 locations `code_dept`
#> 10 time periods `time_period`
#> ── data context ────────────────────────────────────────────────────────────────
#> # A tibble: 850 × 3
#>    code_dept crime_pers time_period
#>  * <fct>          <dbl>       <int>
#>  1 01             4663.           1
#>  2 02            43762.           1
#>  3 03            32137.           1
#>  4 04             4067.           1
#>  5 05              259.           1
#>  6 07             8837.           1
#>  7 08            35047.           1
#>  8 09             3577.           1
#>  9 10            28732.           1
#> 10 11            24175.           1
#> # ℹ 840 more rows
```

The **geometry context** is an sf object that too can be used like any
other sf object and is activated with `activate(x, "geometry")`.

``` r

spt |> 
  activate("geometry") 
#> spacetime ────
#> Context:`geometry`
#> 85 locations `code_dept`
#> 10 time periods `time_period`
#> ── geometry context ────────────────────────────────────────────────────────────
#> Simple feature collection with 85 features and 1 field
#> Geometry type: MULTIPOLYGON
#> Dimension:     XY
#> Bounding box:  xmin: 47680 ymin: 1703258 xmax: 1031401 ymax: 2677441
#> CRS:           NA
#> # A tibble: 85 × 2
#>    code_dept                                                            geometry
#>  * <fct>                                                          <MULTIPOLYGON>
#>  1 01        (((801150 2092615, 800669 2093190, 800688 2095430, 800780 2095795,…
#>  2 02        (((729326 2521619, 729320 2521230, 729280 2518544, 728751 2517520,…
#>  3 03        (((710830 2137350, 711746 2136617, 712430 2135212, 712070 2134132,…
#>  4 04        (((882701 1920024, 882408 1920733, 881778 1921200, 881526 1922332,…
#>  5 05        (((886504 1922890, 885733 1922978, 885479 1923276, 883061 1925266,…
#>  6 07        (((747008 1925789, 746630 1925762, 745723 1925138, 744216 1925236,…
#>  7 08        (((818893 2514767, 818614 2514515, 817900 2514467, 817327 2514945,…
#>  8 09        (((509103 1747787, 508820 1747513, 508154 1747093, 505861 1746627,…
#>  9 10        (((775400 2345600, 775068 2345397, 773587 2345177, 772940 2344780,…
#> 10 11        (((626230 1810121, 626269 1810496, 627494 1811321, 627681 1812424,…
#> # ℹ 75 more rows
```

## Spatio-temporal grids and spacetime

Unlike [spacetime](https://github.com/edzer/spacetime), sfdep does not
make explicit distinctions between spatio-temporal full and sparse
grids. Rather, the approach is more laissez faire. The design of the
spacetime interface is very flexible and is designed to let the user
clean their data with whatever tools are familiar and to their own
specification.

The distinction between sparse and full grids is important when it comes
to analyzing data. For example emerging hot spot analysis requires a
spatio-temporal full-grid. sfdep utilizes the phrase *“spacetime cube”*
as popularized by ESRI to refer to a spatio-temporal full grid.

### Spacetime Cubes

A spacetime object is a spacetime cube if every location has a value for
every time index. Another way of saying this is that each location
contains a regular time-series.

In ESRI terminology, the basic unit of a spacetime cube is a *bin*. A
bin is the unique combination of a location and time index. For each
time index, the collection of every location is called a *time slice*.
In every location, the collection of every bin at each time index is
referred to as a a *bin time-series*.

![](https://pro.arcgis.com/en/pro-app/2.8/tool-reference/space-time-pattern-mining/GUID-0FEECE1A-6B54-44B4-AE49-05E7EA849A8B-web.png)

We can test if an object is a spacetime cube with
[`is_spacetime_cube()`](https://josiahparry.github.io/sfdep/reference/is_spacetime_cube.md)

``` r

is_spacetime_cube(spt)
#> [1] TRUE
```

Here we take a sample of 800 of the 850 rows of `spt` which makes this a
sparse grid.

``` r

sparse_spt <- dplyr::slice_sample(spt, n = 800)

is_spacetime_cube(sparse_spt)
#> ! Number of rows does not equal `n time-periods x n locations`
#> [1] FALSE
```

If an object is a spare spatio-temporal grid we can make it a full one
using
[`complete_spacetime_cube()`](https://josiahparry.github.io/sfdep/reference/complete_spacetime_cube.md).
This works similarly to
\[[`tidyr::complete()`](https://tidyr.tidyverse.org/reference/complete.html)\].
[`complete_spacetime_cube()`](https://josiahparry.github.io/sfdep/reference/complete_spacetime_cube.md)
ensures that there is a row for each combination of location and time.
New rows will contain missing values

``` r

spt_complete <- complete_spacetime_cube(sparse_spt)
#> ! Vars(s) `crime_pers` is missing 50 value(s).

is_spacetime_cube(spt_complete)
#> [1] TRUE
```

One of the conditions of being a spactime cube is that the *time-series
must be regular* (only one observation for each time index). Here we can
create a sample of our data with replacement to create an irregular
time-series at multiple locations.

``` r

set.seed(0)
sparse_spt <- dplyr::slice_sample(spt, n = 800, replace = TRUE)

complete_spacetime_cube(sparse_spt)
#> Error in `complete_spacetime_cube()`:
#> ! Location and time combinations are not unique.
#> ℹ There should only be one observation per time and location combination.
```

This error is informative. We do not have unique bins in our spacetime
data. We can check this.

``` r

dplyr::count(sparse_spt, time_period, code_dept)
#> spacetime ────
#> Context:`data`
#> 85 locations `code_dept`
#> 10 time periods `time_period`
#> ── data context ────────────────────────────────────────────────────────────────
#> # A tibble: 517 × 3
#>    time_period code_dept     n
#>          <int> <fct>     <int>
#>  1           1 01            1
#>  2           1 03            1
#>  3           1 05            1
#>  4           1 07            1
#>  5           1 09            1
#>  6           1 12            1
#>  7           1 16            1
#>  8           1 17            2
#>  9           1 18            1
#> 10           1 21            2
#> # ℹ 507 more rows
```

Spacetime cubes are used for emerging hot spot analysis as below.

``` r

emerging_hotspot_analysis(spt, "crime_pers", threshold = 0.05)
#> # A tibble: 85 × 4
#>    location     tau p_value classification      
#>    <fct>      <dbl>   <dbl> <chr>               
#>  1 01       -0.200    0.474 sporadic hotspot    
#>  2 02        0.111    0.721 sporadic hotspot    
#>  3 03       -0.156    0.592 sporadic hotspot    
#>  4 04        0.200    0.474 sporadic coldspot   
#>  5 05       -0.0667   0.858 sporadic coldspot   
#>  6 07       -0.333    0.210 consecutive coldspot
#>  7 08       -0.378    0.152 sporadic hotspot    
#>  8 09       -0.156    0.592 sporadic coldspot   
#>  9 10        0.333    0.210 sporadic hotspot    
#> 10 11       -0.0222   1     sporadic coldspot   
#> # ℹ 75 more rows
```
