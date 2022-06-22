x <- c(66.22, 22.52, 31.01,  9.47, 30.78, 75.21, 79.26,  8.23, 98.73,
  89.78, 65.19, 54.46)
y <- c(32.54, 22.39, 81.21, 31.02, 60.1 , 58.93,  7.68, 39.93, 77.17,
       42.53, 92.08,  8.48)

df <- data.frame(x, y)

library(sf)
geometry <- st_geometry(st_as_sf(df, coords = c("x", "y")))
center_mean(geometry)

df <- read_sf('/Users/josiahparry/Library/r-miniconda-arm64/envs/geo/lib/python3.8/site-packages/libpysal/examples/virginia/vautm17n_points.shp')


st_geometry(df) |>
  center_median()
  center_mean()

# euclidian median should use the internals of
pracma::geo_median

#' Calculate Euclidean Median Center
#'
#' Given an sfc geometry, calculate the Euclidean Median Center.
#'
#' @details
#'
#' Calculation of the Euclidean median is done using the [Rfast] package. If Rfast is not available, the function will error.
euclidian_median <- function() {

}
