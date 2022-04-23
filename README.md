
<!-- README.md is generated from README.Rmd. Please edit that file -->

# sfdep

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/sfdep)](https://CRAN.R-project.org/package=sfdep)
[![R-CMD-check](https://github.com/JosiahParry/sfdep/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/JosiahParry/sfdep/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

sfdep builds on the great shoulders of spdep package for spatial
dependence. sfdep creates an sf and tidyverse friendly interface to the
package as well as introduces new functionality that is not present in
spdep. sfdep utilizes list columns extensively to make this interface
possible.

## Installation

Install the released version from CRAN

``` r
install.packages("sfdep")
```

You can install the development version of sfdep like so:

``` r
remotes::install_github("josiahparry/sfdep")
```

## Usage

There are three main categories of functionality relating to geometry
neighbors, weights, and local indicators of spatial association (LISAs).

### Neighbors

The most fundamental usage is to find contiguous neighbors from a
polygon. This is done with `st_contiguity()` which, by default creates
queen weights. If rook weights are desired, set `queen = FALSE`.
Additional arguments can be passed to the underlying `spdep::poly2nb()`
via `...`. `st_contiguity()` creates an object of class `nb` as used by
`spdep`.

``` r
library(sf)
library(sfdep)
library(dplyr)

# grab geometry
geo <- st_geometry(guerry)

nb <- st_contiguity(geo)

nb
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 420 
#> Percentage nonzero weights: 5.813149 
#> Average number of links: 4.941176
```

We can identify higher order neighbors with `st_nb_lag()` and the
cumulative higher order neighbors with `st_nb_lag_cumul()`.

``` r
st_nb_lag(nb, 2)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 756 
#> Percentage nonzero weights: 10.46367 
#> Average number of links: 8.894118
st_nb_lag_cumul(nb, 2)
#> Neighbour list object:
#> Number of regions: 85 
#> Number of nonzero links: 1176 
#> Percentage nonzero weights: 16.27682 
#> Average number of links: 13.83529
```

Other point geometry neighbor functions are `st_knn()`,
`st_dist_band()`, `st_nb_dists()`.

### Weights

Polygon weights are created with `st_weights()` (which calls
`spdep::nb2listw`). By default they are row standardized weights.

``` r
wt <- st_weights(nb)

wt[1:2]
#> [[1]]
#> [1] 0.25 0.25 0.25 0.25
#> 
#> [[2]]
#> [1] 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667 0.1666667
```

Other point based weights can be created with `st_nb_dists()`,
`st_kernel_weights()` and `st_inverse_weights()`.

### Local Indicators of Spatial Association (LISAs)

LISAs are created from a combination of neighbors and weights and are
intended to be used inside of a dplyr pipeline. The below is a worked
example of calculating the spatial lag and the local moran.

``` r
g <- guerry %>% 
  mutate(nb = st_contiguity(geometry),
         wt = st_weights(nb))
```

Then calculate the spatial lag with `st_lag()`. Given that we’ve only
modified an sf object, we can visualize this with ggplot2.

``` r
library(ggplot2)

# create spatial lag
g %>% 
  mutate(crime_pers_lag = st_lag(crime_pers, nb, wt)) %>% 
  ggplot(aes(fill = crime_pers_lag)) + 
  geom_sf(lwd = 0.2, color = "black") +
  theme_void()
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

Most users will be interested in local indicators of spatial association
(LISA). Utilize `local_moran()` to do this. `local_moran()` will create
a data frame column which contains a number of informative variables.
For example the cluster that a polygon falls into based on mean, median,
or pysal calculations. This will need to be unnested or certain
variables hoisted.

Create the Local Moran data frame column.

``` r
lisa <- g %>% 
  mutate(moran = local_moran(crime_pers, nb, wt))

pull(lisa, moran) %>% 
  glimpse()
#> Rows: 85
#> Columns: 12
#> $ ii           <dbl> 0.52226452, 0.82801651, 0.80353997, 0.74188966, 0.2311871…
#> $ eii          <dbl> -0.0106720706, 0.0100379803, -0.0394349176, -0.0084513898…
#> $ var_ii       <dbl> 0.405336069, 0.124987091, 0.137583392, 0.246197973, 0.042…
#> $ z_ii         <dbl> 0.8370818, 2.3137121, 2.2726452, 1.5122252, 1.1343366, 1.…
#> $ p_ii         <dbl> 0.402546575, 0.020683518, 0.023047572, 0.130476584, 0.256…
#> $ p_ii_sim     <dbl> 0.428, 0.020, 0.032, 0.140, 0.264, 0.084, 0.612, 0.124, 0…
#> $ p_folded_sim <dbl> 0.214, 0.010, 0.016, 0.070, 0.132, 0.042, 0.306, 0.062, 0…
#> $ skewness     <dbl> 0.014467843, 0.053213846, 0.152412902, -0.073860474, -0.0…
#> $ kurtosis     <dbl> -0.510595407, -0.221248841, -0.059662086, -0.165626033, -…
#> $ mean         <fct> High-High, High-High, High-High, Low-Low, Low-Low, Low-Lo…
#> $ median       <fct> High-High, High-High, High-High, Low-Low, Low-Low, Low-Lo…
#> $ pysal        <fct> High-High, High-High, High-High, Low-Low, Low-Low, Low-Lo…
```

Visualize this.

``` r
lisa %>% 
  tidyr::unnest(moran) %>% 
  ggplot(aes(fill = mean)) +
  geom_sf() +
  geom_sf(lwd = 0.2, color = "black") +
  theme_void() +
  scale_fill_manual(values = c("#1C4769", "#24975E", "#EACA97", "#B20016"))
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

<!-- ## Neighbor apply function -->
<!-- In the case that you want to create custom local level metrics, you can do so with the function `st_nb_apply()`. This provides a purrr like interface to do calculation on an observation $x_{i}$ using it's neighbors, $x_{ij}$, and their respective weights $w_{ij}$. In the formula syntax utilize `.xij`, `.nb` and `.wt` for these values respectively. We can specify the type of output with the suffix argument. By default this will be `"dbl"`. These can be any purrr sufix. To get the default list output use `"list"`.  -->
<!-- For example  -->
<!-- ```{r} -->
<!-- g %>%  -->
<!--   transmute(x = st_nb_apply(crime_pers, nb, wt,  -->
<!--                             .f = function(.xij, .wt, ...) sum((.xij - crime_pers)^2) / (length(.wt) + 1), suffix = "dbl")) -->
<!-- ``` -->
